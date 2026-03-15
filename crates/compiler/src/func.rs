use cranelift::{
    codegen::{
        Context,
        ir::{self, UserFuncName},
    },
    jit::JITModule,
    module::{FuncId, Linkage, Module},
    prelude::{Block, FunctionBuilder, FunctionBuilderContext, InstBuilder},
};
use indexmap::{IndexMap, map::Entry};
use mollie_ir::MollieType;
use mollie_shared::Operator;
use mollie_typed_ast::{ExprRef, ModuleLoader, TypedASTContext};
use mollie_typing::{AdtVariantRef, FuncRef, Type, TypeRef, VFuncRef, VTableRef};

use crate::{
    Compiler, FuncCompilerError, MolValue, Var,
    allocator::TypeLayout,
    error::{CompileError, CompileResult},
};

#[derive(Debug, Clone, Copy)]
pub struct FunctionContext {
    pub alloc: ir::FuncRef,
    pub alloc_array: ir::FuncRef,
    pub realloc_array: ir::FuncRef,
    pub mark_root: ir::FuncRef,
    pub unmark_root: ir::FuncRef,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum FuncKey {
    VFunc(u64, VTableRef, VFuncRef),
    Ref(FuncRef),
    Id(FuncId),
}

#[derive(Debug, Clone, Copy)]
pub struct Variable {
    pub value: Var,
    pub ty: TypeRef,
    pub captured: bool,
}

impl Variable {
    pub const fn new(var: Var, ty: TypeRef) -> Self {
        Self {
            value: var,
            ty,
            captured: false,
        }
    }
}

pub type VariableFrame = IndexMap<String, Variable>;

#[derive(Debug)]
pub struct Frames {
    frames: Vec<VariableFrame>,
}

impl Frames {
    pub fn new() -> Self {
        Self {
            frames: vec![VariableFrame::new()],
        }
    }

    pub fn push(&mut self) {
        self.frames.push(IndexMap::new());
    }

    pub fn pop(&mut self) {
        self.frames.pop();
    }

    pub fn get_var<T: AsRef<str>>(&self, name: T) -> Option<Var> {
        let name = name.as_ref();

        for frame in self.frames.iter().rev() {
            if let Some(var) = frame.get(name) {
                return Some(var.value);
            }
        }

        None
    }

    pub fn current(&self) -> &VariableFrame {
        let last_frame = self.frames.len() - 1;

        &self.frames[last_frame]
    }

    pub fn current_mut(&mut self) -> &mut VariableFrame {
        let last_frame = self.frames.len() - 1;

        &mut self.frames[last_frame]
    }
}

impl Default for Frames {
    fn default() -> Self {
        Self::new()
    }
}

pub struct FunctionCompiler<'a, S, ML: ModuleLoader<S>, M: Module = JITModule> {
    pub(crate) id: FuncId,
    pub(crate) entry_block: Block,

    pub(crate) compiler: &'a mut Compiler<M>,
    pub(crate) fn_builder: FunctionBuilder<'a>,
    pub(crate) type_context: &'a TypedASTContext<S, ML>,

    pub(crate) context: FunctionContext,

    pub(crate) this: Option<MolValue>,
    pub(crate) assign_ref: Option<(ExprRef, Operator, ExprRef)>,
    pub(crate) branches: Option<(ir::Block, ir::Block)>,

    pub(crate) funcs: IndexMap<FuncKey, ir::FuncRef>,
    pub(crate) frames: Frames,
}

impl<'a, S, ML: ModuleLoader<S>, M: Module> FunctionCompiler<'a, S, ML, M> {
    pub fn new<T: Into<String>>(
        name: T,
        signature: ir::Signature,
        compiler: &'a mut Compiler<M>,
        type_context: &'a TypedASTContext<S, ML>,
        ctx: &'a mut Context,
        fn_builder_ctx: &'a mut FunctionBuilderContext,
    ) -> Result<Self, FuncCompilerError> {
        compiler.codegen.module.clear_context(ctx);

        let name = name.into();

        ctx.func.name = UserFuncName::testcase(&name);

        let id = compiler.codegen.module.declare_function(&name, Linkage::Local, &signature)?;
        let mut fn_builder = FunctionBuilder::new(&mut ctx.func, fn_builder_ctx);
        let context = FunctionContext {
            alloc: match compiler.name_to_func_id.get("molalloc") {
                Some(&func_id) => compiler.codegen.module.declare_func_in_func(func_id, fn_builder.func),
                None => return Err(FuncCompilerError::AllocNotFound),
            },
            alloc_array: match compiler.name_to_func_id.get("molalloc_arr") {
                Some(&func_id) => compiler.codegen.module.declare_func_in_func(func_id, fn_builder.func),
                None => return Err(FuncCompilerError::AllocNotFound),
            },
            realloc_array: match compiler.name_to_func_id.get("molrealloc_arr") {
                Some(&func_id) => compiler.codegen.module.declare_func_in_func(func_id, fn_builder.func),
                None => return Err(FuncCompilerError::AllocNotFound),
            },
            mark_root: match compiler.name_to_func_id.get("molmark_root") {
                Some(&func_id) => compiler.codegen.module.declare_func_in_func(func_id, fn_builder.func),
                None => return Err(FuncCompilerError::AllocNotFound),
            },
            unmark_root: match compiler.name_to_func_id.get("molunmark_root") {
                Some(&func_id) => compiler.codegen.module.declare_func_in_func(func_id, fn_builder.func),
                None => return Err(FuncCompilerError::AllocNotFound),
            },
        };

        fn_builder.func.signature = signature;

        let entry_block = fn_builder.create_block();

        fn_builder.append_block_params_for_function_params(entry_block);
        fn_builder.switch_to_block(entry_block);
        fn_builder.seal_block(entry_block);

        compiler.func_id_to_name.insert(id, name);

        Ok(FunctionCompiler {
            id,
            entry_block,
            compiler,
            fn_builder,
            type_context,
            context,
            this: None,
            assign_ref: None,
            branches: None,
            funcs: IndexMap::new(),
            frames: Frames::new(),
        })
    }

    pub fn new_anonymous(
        signature: ir::Signature,
        compiler: &'a mut Compiler<M>,
        checker: &'a TypedASTContext<S, ML>,
        ctx: &'a mut Context,
        fn_builder_ctx: &'a mut FunctionBuilderContext,
    ) -> Result<Self, FuncCompilerError> {
        compiler.codegen.module.clear_context(ctx);

        let id = compiler.codegen.module.declare_anonymous_function(&signature)?;
        let mut fn_builder = FunctionBuilder::new(&mut ctx.func, fn_builder_ctx);
        let context = FunctionContext {
            alloc: match compiler.name_to_func_id.get("molalloc") {
                Some(&func_id) => compiler.codegen.module.declare_func_in_func(func_id, fn_builder.func),
                None => return Err(FuncCompilerError::AllocNotFound),
            },
            alloc_array: match compiler.name_to_func_id.get("molalloc_arr") {
                Some(&func_id) => compiler.codegen.module.declare_func_in_func(func_id, fn_builder.func),
                None => return Err(FuncCompilerError::AllocNotFound),
            },
            realloc_array: match compiler.name_to_func_id.get("molrealloc_arr") {
                Some(&func_id) => compiler.codegen.module.declare_func_in_func(func_id, fn_builder.func),
                None => return Err(FuncCompilerError::AllocNotFound),
            },
            mark_root: match compiler.name_to_func_id.get("molmark_root") {
                Some(&func_id) => compiler.codegen.module.declare_func_in_func(func_id, fn_builder.func),
                None => return Err(FuncCompilerError::AllocNotFound),
            },
            unmark_root: match compiler.name_to_func_id.get("molunmark_root") {
                Some(&func_id) => compiler.codegen.module.declare_func_in_func(func_id, fn_builder.func),
                None => return Err(FuncCompilerError::AllocNotFound),
            },
        };

        fn_builder.func.signature = signature;

        let entry_block = fn_builder.create_block();

        fn_builder.append_block_params_for_function_params(entry_block);
        fn_builder.switch_to_block(entry_block);
        fn_builder.seal_block(entry_block);

        Ok(FunctionCompiler {
            id,
            entry_block,
            branches: None,
            compiler,
            fn_builder,
            type_context: checker,
            context,
            this: None,
            assign_ref: None,
            funcs: IndexMap::new(),
            frames: Frames::new(),
        })
    }

    pub fn get_var<T: AsRef<str>>(&self, name: T) -> Option<Var> {
        self.frames.get_var(name)
    }

    pub fn current_frame(&self) -> &VariableFrame {
        self.frames.current()
    }

    pub fn current_frame_mut(&mut self) -> &mut VariableFrame {
        self.frames.current_mut()
    }

    pub fn push_frame(&mut self) {
        self.frames.push();
    }

    pub fn pop_frame(&mut self) {
        self.frames.pop();
    }

    pub fn unmark_variables(&mut self) {
        for (_, &var) in self.frames.current() {
            if matches!(self.type_context.type_context.types[var.ty], Type::Adt(..))
                && let Var::Regular(ptr) = var.value
                && !var.captured
            {
                let ptr = self.fn_builder.use_var(ptr);

                self.fn_builder.ins().call(self.context.unmark_root, &[ptr]);
            }
        }
    }

    pub fn hash_of(&self, ty: TypeRef) -> u64 {
        self.type_context.type_context.types.hash_of(ty)
    }

    pub fn alloc(&mut self, type_layout: *const TypeLayout) -> ir::Value {
        let type_layout_ptr = self.fn_builder.ins().iconst(self.compiler.ptr_type(), type_layout as i64);
        let alloc_call = self.fn_builder.ins().call(self.context.alloc, &[type_layout_ptr]);

        self.fn_builder.inst_results(alloc_call)[0]
    }

    pub fn var<T: Into<String>>(&mut self, name: T, ty: ir::Type, type_info: TypeRef, value: ir::Value) {
        let var = self.fn_builder.declare_var(ty);

        self.current_frame_mut().insert(name.into(), Variable::new(Var::Regular(var), type_info));
        self.fn_builder.def_var(var, value);
    }

    pub fn fat_var<T: Into<String>>(&mut self, name: T, ty: ir::Type, fat_ty: ir::Type, type_info: TypeRef, value: ir::Value, metadata: ir::Value) {
        let var = self.fn_builder.declare_var(ty);
        let fat_var = self.fn_builder.declare_var(fat_ty);

        self.current_frame_mut().insert(name.into(), Variable::new(Var::Fat(var, fat_var), type_info));
        self.fn_builder.def_var(var, value);
        self.fn_builder.def_var(fat_var, metadata);
    }

    pub fn return_(&mut self, value: MolValue) -> ir::Inst {
        match value {
            MolValue::Value(value) => self.fn_builder.ins().return_(&[value]),
            MolValue::Values(values) => self.fn_builder.ins().return_(&values),
            MolValue::FuncRef(func_ref) => {
                let ptr_type = self.compiler.ptr_type();
                let func_addr = self.fn_builder.ins().func_addr(ptr_type, func_ref);

                self.fn_builder.ins().return_(&[func_addr])
            }
            MolValue::CaptureFuncRef(func_ref, _captures) => {
                let ptr_type = self.compiler.ptr_type();
                let func_addr = self.fn_builder.ins().func_addr(ptr_type, func_ref);

                self.fn_builder.ins().return_(&[func_addr])
            }
            MolValue::FatPtr(ptr, metadata) => self.fn_builder.ins().return_(&[ptr, metadata]),
            MolValue::Nothing => self.fn_builder.ins().return_(&[]),
        }
    }

    pub fn construct(&mut self, ty: TypeRef, variant: AdtVariantRef, values: &[ir::Value]) -> CompileResult<ir::Value> {
        let mut values = values.iter().copied();

        let hash = self.hash_of(ty);
        let type_layout = &raw const *self.compiler.adt_types[&hash].type_layout;
        let ptr = self.alloc(type_layout);

        let variant = self.compiler.get_adt_variant(hash, variant);

        let expected = variant.fields.values().map(|(f, _)| if f.ty.is_fat() { 2 } else { 1 }).sum();
        let mut found = 0;

        for (field, _) in variant.fields.values() {
            match field.ty {
                MollieType::Regular(_) => match values.next() {
                    Some(value) => {
                        self.fn_builder.ins().store(ir::MemFlags::trusted(), value, ptr, field.offset);

                        found += 1;
                    }
                    None => return Err(CompileError::AdtArgsCount { expected, found }),
                },
                MollieType::Fat(ty, _) => match (values.next(), values.next()) {
                    (Some(value), Some(metadata)) => {
                        self.fn_builder.ins().store(ir::MemFlags::trusted(), value, ptr, field.offset);
                        self.fn_builder
                            .ins()
                            .store(ir::MemFlags::trusted(), metadata, ptr, field.offset + ty.bytes().cast_signed());

                        found += 2;
                    }
                    _ => return Err(CompileError::AdtArgsCount { expected, found }),
                },
            }
        }

        found += values.count();

        if found != expected {
            return Err(CompileError::AdtArgsCount { expected, found });
        }

        Ok(ptr)
    }

    pub fn get_vfunc(&mut self, hash: u64, vtable: VTableRef, func: VFuncRef) -> ir::FuncRef {
        match self.funcs.entry(FuncKey::VFunc(hash, vtable, func)) {
            Entry::Occupied(vfunc) => *vfunc.get(),
            Entry::Vacant(vfunc) => {
                let func = self.compiler.codegen.module.declare_func_in_func(
                    self.compiler
                        .vtables
                        .get(&(hash, vtable))
                        .unwrap_or_else(|| panic!("there's no vtable for ({hash}, {vtable:?})"))[func],
                    self.fn_builder.func,
                );

                *vfunc.insert(func)
            }
        }
    }
}
