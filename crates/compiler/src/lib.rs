#![allow(clippy::result_large_err)]

use std::{
    hash::{DefaultHasher, Hash, Hasher},
    iter::repeat,
    mem,
};

pub use cranelift;
use cranelift::{
    codegen::{Context, ir, print_errors::pretty_error, write::decorate_function},
    jit::JITModule,
    module::{DataId, FuncId, Linkage, Module, ModuleError, ModuleResult},
    prelude::{
        AbiParam, Configurable, FloatCC, FunctionBuilder, FunctionBuilderContext, InstBuilder, IntCC, Signature, Variable, isa::TargetIsa, settings, types,
    },
};
pub use indexmap::IndexMap;
use indexmap::map::Entry;
pub use itertools;
use itertools::Itertools;
use mollie_index::{Idx, IndexVec};
use mollie_ir::{Array, CodeGenerator, Field, MollieType, Symbol, VTablePtr, compile_constant};
use mollie_lexer::{Lexer, Token};
use mollie_parser::{BlockExpr, Parser, parse_statements_until};
use mollie_shared::{Operator, Span};
use mollie_tast::{
    BlockRef, Expr, ExprRef, Func, FuncRef, IntoTypedAST, IsPattern, LiteralExpr, Module as TypeModule, ModuleId, ModuleItem, Stmt, StmtRef, TypeChecker,
    TypeErrorDisplay, TypePath, TypedAST, VFunc, VTableFuncKind,
};
use mollie_typing::{
    CompiledComplexType, CompiledComplexTypeVariant, ComplexType, ComplexTypeKind, ComplexTypeRef, ComplexTypeVariantRef, FieldRef, PrimitiveType, TraitRef,
    TypeInfo, TypeInfoRef, TypeSolver, TypeUnificationError, VFuncRef, VTableRef,
};

pub use self::error::{CompileError, CompileResult};
use crate::comment_writer::CommentWriter;

mod comment_writer;
mod error;

#[derive(Debug)]
pub enum Var {
    Regular(Variable),
    Fat(Variable, Variable),
}

#[derive(Debug)]
pub struct Compiler<T: Module = JITModule> {
    complex_types: IndexMap<u64, CompiledComplexType>,
    trait_to_vtable: IndexMap<(u64, Option<TraitRef>), DataId>,
    vtables: IndexMap<(u64, VTableRef), IndexVec<VFuncRef, (ir::SigRef, ir::FuncRef, FuncId)>>,
    vfuncs: IndexMap<(u64, VTableRef, VFuncRef), ir::FuncRef>,
    current_funcs: IndexMap<FuncRef, ir::FuncRef>,
    funcs: IndexMap<FuncRef, (ir::SigRef, ir::FuncRef, FuncId)>,

    assign_ref: Option<(ExprRef, ExprRef)>,
    this: Option<ValueOrFunc>,
    values: IndexMap<String, ValueOrFunc>,
    pub variables: IndexMap<String, Var>,
    globals: IndexMap<String, FuncId>,
    pub func_names: IndexMap<FuncId, String>,

    codegen: CodeGenerator<T>,

    pub comment_writer: CommentWriter,
}

impl Compiler {
    fn import_fn<T: IntoIterator<Item = ir::Type>>(&mut self, name: &str, params: T) -> ModuleResult<FuncId> {
        let mut signature = self.codegen.module.make_signature();

        signature.params.extend(params.into_iter().map(AbiParam::new));

        let id = self.codegen.module.declare_function(name, Linkage::Import, &signature)?;

        self.func_names.insert(id, name.to_owned());
        self.globals.insert(name.to_owned(), id);

        Ok(id)
    }

    fn try_import_vfunc(&mut self, hash: u64, vtable: VTableRef, func: VFuncRef, fn_builder: &mut FunctionBuilder) -> ir::FuncRef {
        match self.vfuncs.entry((hash, vtable, func)) {
            Entry::Occupied(occupied_entry) => *occupied_entry.get(),
            Entry::Vacant(vacant_entry) => {
                let func = self.codegen.module.declare_func_in_func(self.vtables[&(hash, vtable)][func].2, fn_builder.func);

                *vacant_entry.insert(func)
            }
        }
    }

    pub const fn codegen(&self) -> &CodeGenerator<JITModule> {
        &self.codegen
    }

    pub const fn codegen_mut(&mut self) -> &mut CodeGenerator<JITModule> {
        &mut self.codegen
    }

    /// Gets a pointer to the compiled function with the specified `name` and
    /// `transmute`s it to `T`.
    ///
    /// # Safety
    ///
    /// `T` must be a function type and be the same size as pointers, otherwise
    /// you will get undefined behavior.
    pub unsafe fn get_func<T>(&self, name: impl AsRef<str>) -> Option<T> {
        debug_assert_eq!(mem::size_of::<T>(), mem::size_of::<*const u8>());

        self.globals.get(name.as_ref()).map(|&func_id| {
            let code = self.codegen.module.get_finalized_function(func_id);

            unsafe { mem::transmute_copy::<mem::ManuallyDrop<*const u8>, T>(&mem::ManuallyDrop::new(code)) }
        })
    }
}

pub struct FuncCompiler<'a> {
    pub compiler: &'a mut Compiler,
    pub ctx: Context,
    pub fn_builder_ctx: FunctionBuilderContext,
    pub checker: TypeChecker,
}

impl FuncCompiler<'_> {
    /// Creates a compiler for the main function, returning [`FuncCompiler`].
    ///
    /// # Errors
    ///
    /// Returns [`ModuleError`] in case of an error during function declaration
    /// or definition.
    ///
    /// [`ModuleError`]: cranelift::module::ModuleError
    pub fn compile<T: AsRef<str>>(&mut self, name: T, params: Vec<(String, ir::Type)>, text: T, dump: bool) -> CompileResult<FuncId> {
        self.compiler.codegen.module.clear_context(&mut self.ctx);

        let mut fn_builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.fn_builder_ctx);

        for &(_, ty) in &params {
            fn_builder.func.signature.params.push(ir::AbiParam::new(ty));
        }

        let entry_block = fn_builder.create_block();

        fn_builder.append_block_params_for_function_params(entry_block);
        fn_builder.switch_to_block(entry_block);
        fn_builder.seal_block(entry_block);

        for (i, (arg_name, ty)) in params.into_iter().enumerate() {
            let value = fn_builder.block_params(entry_block)[i];
            let var = fn_builder.declare_var(ty);

            self.compiler.variables.insert(arg_name, Var::Regular(var));

            fn_builder.def_var(var, value);
        }

        let mut parser = Parser::new(Lexer::lex(text.as_ref()));
        let ptr_type = self.compiler.codegen.module.isa().pointer_type();

        let program = match parse_statements_until(&mut parser, &Token::EOF) {
            Ok(statements) => statements,
            Err(error) => return Err(CompileError::Parse(error)),
        };

        let mut ast = TypedAST::default();

        let block = BlockExpr {
            stmts: program.0,
            final_stmt: program.1.map(Box::new),
        }
        .into_typed_ast(&mut self.checker, &mut ast, Span::default())
        .unwrap();

        self.checker.solver.finalize();

        for &func_ref in &ast.used_functions {
            let func = &self.checker.local_functions[func_ref];

            if !self.compiler.funcs.contains_key(&func_ref) {
                let mut signature = self.compiler.codegen.module.make_signature();

                if let TypeInfo::Func(args, returns) = self.checker.solver.get_info(func.ty) {
                    for arg in args {
                        if arg.as_inner() != &self.checker.core_types.void {
                            arg.as_inner()
                                .as_ir_type(&self.checker.solver, self.compiler.codegen.module.isa())
                                .add_to_params(&mut signature.params);
                        }
                    }

                    if returns != &self.checker.core_types.void {
                        returns
                            .as_ir_type(&self.checker.solver, self.compiler.codegen.module.isa())
                            .add_to_params(&mut signature.returns);
                    }
                }

                match &func.kind {
                    VTableFuncKind::Local(_) => todo!(),
                    VTableFuncKind::External(name) => {
                        let func_id = self.compiler.codegen.module.declare_function(name, Linkage::Import, &signature).unwrap();

                        let signature = fn_builder.import_signature(signature);
                        let func = self.compiler.codegen.module.declare_func_in_func(func_id, fn_builder.func);

                        self.compiler.funcs.insert(func_ref, (signature, func, func_id));
                    }
                }
            }
        }

        for (complex_type, args) in &ast.used_complex_types {
            if args.iter().any(|arg| self.checker.solver.contains_unknown(*arg)) || args.len() < self.checker.complex_types[*complex_type].generics {
                continue;
            }

            let kind = self.checker.complex_types[*complex_type].kind;
            let hash = {
                let mut state = DefaultHasher::new();

                "complex".hash(&mut state);

                complex_type.hash(&mut state);
                kind.hash(&mut state);

                for &type_arg in args {
                    self.checker.solver.hash_into(&mut state, type_arg);
                }

                state.finish()
            };

            if self.compiler.complex_types.contains_key(&hash) {
                continue;
            }

            println!(
                "Compiled complex type at `{}` => `{hash}`",
                self.checker.complex_types[*complex_type].name.as_deref().unwrap_or_default()
            );

            let mut size = 0;

            self.compiler.complex_types.insert(hash, CompiledComplexType {
                variants: self.checker.complex_types[*complex_type]
                    .variants
                    .values()
                    .map(|variant| {
                        let mut fields = <IndexVec<FieldRef, (Field, TypeInfoRef)>>::with_capacity(variant.fields.len());
                        let mut offset = 0;
                        let mut variant_size = 0;
                        let mut self_align = 0;

                        for (_, field, default_value) in variant.fields.values() {
                            let type_info = field.as_type_info(None, &self.checker.core_types, &mut self.checker.solver, args.as_ref());
                            let ty = type_info.as_ir_type(&self.checker.solver, self.compiler.codegen.module.isa());
                            let ty_size = ty.bytes();

                            variant_size += ty_size;

                            let align = ty_size;
                            let padding = (align - variant_size % align) % align;

                            variant_size += padding;

                            fields.push((
                                Field {
                                    ty,
                                    offset,
                                    default_value: default_value.clone(),
                                },
                                type_info,
                            ));

                            offset += ty_size.cast_signed();

                            let align = ty_size.cast_signed();
                            let padding = (align - offset % align) % align;

                            offset += padding;
                            self_align = self_align.max(ty_size);
                        }

                        if !variant.fields.is_empty() {
                            size = size.max(variant_size + (self_align - variant_size % self_align) % self_align);
                        }

                        CompiledComplexTypeVariant {
                            fields: fields.into_boxed_slice(),
                        }
                    })
                    .collect(),
                size,
            });
        }

        for (target_ty, vtable, args) in &ast.used_vtables {
            let target_ty = *target_ty;
            let vtable = *vtable;
            let hash = self.checker.solver.hash_of(target_ty);

            if self.compiler.vtables.contains_key(&(hash, vtable)) {
                continue;
            }

            let args = self.checker.vtables[vtable]
                .generics
                .iter()
                .zip(args)
                .map(|(g, a)| {
                    let val = self.checker.solver.type_infos[*a].clone();

                    (*g, std::mem::replace(&mut self.checker.solver.type_infos[*g], val))
                })
                .collect::<Box<[_]>>();

            let args_fmt = args.iter().map(|(arg, _)| self.checker.display_of_type(*arg, None)).join("");

            let functions = self.checker.vtables[vtable]
                .functions
                .values()
                .map(|func| {
                    let mut signature = self.compiler.codegen.module.make_signature();

                    if let TypeInfo::Func(args, returns) = self.checker.solver.get_info(func.ty) {
                        for arg in args {
                            if arg.as_inner() != &self.checker.core_types.void {
                                arg.as_inner()
                                    .as_ir_type(&self.checker.solver, self.compiler.codegen.module.isa())
                                    .add_to_params(&mut signature.params);
                            }
                        }

                        if returns != &self.checker.core_types.void {
                            returns
                                .as_ir_type(&self.checker.solver, self.compiler.codegen.module.isa())
                                .add_to_params(&mut signature.returns);
                        }
                    }

                    match &func.kind {
                        VTableFuncKind::Local(body) => {
                            let current_funcs = std::mem::take(&mut self.compiler.current_funcs);
                            let vfuncs = std::mem::take(&mut self.compiler.vfuncs);
                            let values = mem::take(&mut self.compiler.values);

                            let name = format!("{vtable:?}_{args_fmt}_{}", func.name);

                            let mut ctx = self.compiler.codegen.module.make_context();
                            let func_id = self
                                .compiler
                                .codegen
                                .module
                                .declare_function(
                                    &name,
                                    Linkage::Local, // if self.value.func_vis.is_some() { Linkage::Export } else { Linkage::Local },
                                    &signature,
                                )
                                .unwrap();

                            self.compiler.func_names.insert(func_id, name);

                            ctx.func.signature = signature.clone();

                            let mut fn_builder_ctx = FunctionBuilderContext::new();
                            let mut temp_fn_builder = FunctionBuilder::new(&mut ctx.func, &mut fn_builder_ctx);

                            let entry_block = temp_fn_builder.create_block();

                            temp_fn_builder.append_block_params_for_function_params(entry_block);
                            temp_fn_builder.switch_to_block(entry_block);
                            temp_fn_builder.seal_block(entry_block);

                            let mut index = 0;

                            for (arg, name) in func.arg_names.iter().enumerate() {
                                let value = temp_fn_builder.block_params(entry_block)[index];
                                let ty = temp_fn_builder.func.signature.params[index].value_type;

                                let var = temp_fn_builder.declare_var(ty);

                                temp_fn_builder.def_var(var, value);

                                if let TypeInfo::Func(args, _) = self.checker.solver.get_info(func.ty)
                                    && args[arg]
                                        .as_inner()
                                        .as_ir_type(&self.checker.solver, self.compiler.codegen.module.isa())
                                        .is_fat()
                                {
                                    let value = temp_fn_builder.block_params(entry_block)[index + 1];
                                    let ty = temp_fn_builder.func.signature.params[index + 1].value_type;

                                    let metadata_var = temp_fn_builder.declare_var(ty);

                                    temp_fn_builder.def_var(metadata_var, value);

                                    self.compiler.variables.insert(name.clone(), Var::Fat(var, metadata_var));

                                    index += 2;
                                } else {
                                    self.compiler.variables.insert(name.clone(), Var::Regular(var));

                                    index += 1;
                                }
                            }

                            let result = body.compile(&ast, &self.checker, self.compiler, &mut temp_fn_builder).unwrap();

                            match result {
                                ValueOrFunc::Value(value) => temp_fn_builder.ins().return_(&[value]),
                                ValueOrFunc::Values(values) => temp_fn_builder.ins().return_(&values),
                                ValueOrFunc::FuncRef(func_ref) => {
                                    let func_addr = temp_fn_builder.ins().func_addr(ptr_type, func_ref);

                                    temp_fn_builder.ins().return_(&[func_addr])
                                }
                                ValueOrFunc::FatPtr(ptr, metadata) => temp_fn_builder.ins().return_(&[ptr, metadata]),
                                ValueOrFunc::Nothing => temp_fn_builder.ins().return_(&[]),
                            };

                            self.compiler.codegen.module.define_function(func_id, &mut ctx).unwrap();
                            self.compiler.codegen.module.clear_context(&mut ctx);
                            self.compiler.codegen.module.finalize_definitions().unwrap();

                            for name in &func.arg_names {
                                self.compiler.variables.shift_remove(name);
                            }

                            let signature = fn_builder.import_signature(signature);
                            let func = self.compiler.codegen.module.declare_func_in_func(func_id, fn_builder.func);

                            self.compiler.values = values;
                            self.compiler.vfuncs = vfuncs;
                            self.compiler.current_funcs = current_funcs;

                            (signature, func, func_id)
                        }
                        VTableFuncKind::External(name) => {
                            let func_id = self.compiler.codegen.module.declare_function(name, Linkage::Import, &signature).unwrap();

                            let signature = fn_builder.import_signature(signature);
                            let func = self.compiler.codegen.module.declare_func_in_func(func_id, fn_builder.func);

                            (signature, func, func_id)
                        }
                    }
                })
                .collect();

            for (g, g_val) in args {
                self.checker.solver.type_infos[g] = g_val;
            }

            println!(
                "Compiled vtable at (`{}` => `{hash}`, `{vtable:?}`)",
                self.checker.display_of_type(target_ty, None)
            );

            self.compiler.vtables.insert((hash, vtable), functions);
        }

        for (target_ty, vtable, _) in &ast.used_vtables {
            let hash = self.checker.solver.hash_of(*target_ty);
            let trait_ref = self.checker.vtables[*vtable].origin_trait;

            if self.compiler.trait_to_vtable.contains_key(&(hash, trait_ref)) {
                continue;
            }

            let size = Array {
                element: MollieType::Regular(self.compiler.codegen.module.isa().pointer_type()),
            }
            .get_size(self.compiler.vtables[&(hash, *vtable)].len() + 1);

            self.compiler.codegen.data_desc.define_zeroinit(size as usize);

            let id = self.compiler.codegen.module.declare_anonymous_data(true, false).unwrap();

            self.compiler.codegen.module.define_data(id, &self.compiler.codegen.data_desc).unwrap();
            self.compiler.codegen.data_desc.clear();

            let ptr_type = self.compiler.codegen.module.isa().pointer_type();

            let data_id = self.compiler.codegen.module.declare_data_in_func(id, fn_builder.func);
            let vtable_ptr = fn_builder.ins().global_value(ptr_type, data_id);

            let type_id = fn_builder.ins().iconst(ptr_type, hash as i64);

            self.compiler.comment_writer.add_post_comment(
                fn_builder.ins().store(ir::MemFlags::trusted(), type_id, vtable_ptr, 0),
                "type id stored in vtable",
            );

            let mut offset = ptr_type.bytes();

            for (_, func_ref, _) in self.compiler.vtables[&(hash, *vtable)].values() {
                let func_ptr = fn_builder.ins().func_addr(ptr_type, *func_ref);

                self.compiler.comment_writer.add_post_comment(
                    fn_builder.ins().store(ir::MemFlags::trusted(), func_ptr, vtable_ptr, offset.cast_signed()),
                    "function ptr stored in vtable",
                );

                offset += ptr_type.bytes();
            }

            self.compiler.trait_to_vtable.insert((hash, trait_ref), id);
        }

        match block.compile(&ast, &self.checker, self.compiler, &mut fn_builder).unwrap() {
            ValueOrFunc::Value(value) => fn_builder.ins().return_(&[value]),
            ValueOrFunc::Values(values) => fn_builder.ins().return_(&values),
            ValueOrFunc::FuncRef(func_ref) => {
                let func_addr = fn_builder.ins().func_addr(ptr_type, func_ref);

                fn_builder.ins().return_(&[func_addr])
            }
            ValueOrFunc::FatPtr(ptr, metadata) => fn_builder.ins().return_(&[ptr, metadata]),
            ValueOrFunc::Nothing => fn_builder.ins().return_(&[]),
        };

        let name = name.as_ref();
        let func = self
            .compiler
            .codegen
            .module
            .declare_function(name, Linkage::Export, &fn_builder.func.signature)
            .unwrap();

        fn_builder.finalize();

        if let Err(e) = self.compiler.codegen.module.define_function(func, &mut self.ctx) {
            match e {
                ModuleError::Compilation(error) => {
                    panic!("function definition failed: {}", pretty_error(&self.ctx.func, error));
                }
                e => panic!("{}\nfunction definition failed: {e}", self.ctx.func),
            }
        }

        if dump {
            let mut data = String::new();

            decorate_function(&mut &self.compiler.comment_writer, &mut data, &self.ctx.func).unwrap();

            println!("{data}");
        }

        self.compiler.codegen.module.finalize_definitions().unwrap();
        self.compiler.globals.insert(name.into(), func);

        self.compiler.values.clear();
        self.compiler.vfuncs.clear();
        self.compiler.current_funcs.clear();

        Ok(func)
    }

    pub fn type_errors(&mut self) -> impl Iterator<Item = TypeErrorDisplay<'_>> {
        mem::take(&mut self.checker.solver.errors)
            .into_iter()
            .map(|error| self.checker.display_of_error(error))
    }

    pub fn register_module<T: Into<String>>(&mut self, name: T) -> ModuleId {
        let id = self.checker.modules.next_index();
        let name = name.into();

        self.checker.modules[ModuleId::ZERO].items.insert(name.clone(), ModuleItem::SubModule(id));
        self.checker.modules.insert(TypeModule {
            parent: Some(ModuleId::ZERO),
            name,
            id,
            items: IndexMap::new(),
        })
    }

    pub fn register_complex_type(&mut self, ty: ComplexType) -> ComplexTypeRef {
        let type_ref = self.checker.complex_types.insert(ty);

        if let Some(name) = self.checker.complex_types[type_ref].name.clone() {
            self.checker.modules[ModuleId::ZERO].items.insert(name, ModuleItem::ComplexType(type_ref));
        }

        type_ref
    }

    pub fn register_complex_type_in_module(&mut self, module: ModuleId, ty: ComplexType) -> ComplexTypeRef {
        let type_ref = self.checker.complex_types.insert(ty);

        if let Some(name) = self.checker.complex_types[type_ref].name.clone() {
            self.checker.modules[module].items.insert(name, ModuleItem::ComplexType(type_ref));
        }

        type_ref
    }

    pub fn register_func_in_module(&mut self, module: ModuleId, func: Func) -> FuncRef {
        let func = self.checker.local_functions.insert(func);

        self.checker.modules[module]
            .items
            .insert(self.checker.local_functions[func].name.clone(), ModuleItem::Func(func));

        func
    }

    pub fn instantiate_complex_type<I: IntoIterator<Item = TypeInfoRef>>(&mut self, ty: ComplexTypeRef, generic_args: I) -> TypeInfoRef {
        let kind = self.checker.complex_types[ty].kind;
        let generics = self.checker.complex_types[ty].generics;
        let args = generic_args
            .into_iter()
            .map(Some)
            .chain(repeat(None))
            .take(generics)
            .map(|arg| arg.unwrap_or_else(|| self.checker.solver.add_info(TypeInfo::Unknown(None))))
            .collect();

        self.checker.solver.add_info(TypeInfo::Complex(ty, kind, args))
    }
}

impl Compiler {
    pub fn with_symbols<I: IntoIterator<Item = Symbol>>(symbols: I) -> ModuleResult<Self> {
        let mut flag_builder = settings::builder();

        unsafe {
            flag_builder.set("use_colocated_libcalls", "false").unwrap_unchecked();
            flag_builder.set("opt_level", "speed").unwrap_unchecked();
            flag_builder.set("is_pic", "false").unwrap_unchecked();
        }

        let mut compiler = Self {
            complex_types: IndexMap::new(),
            vtables: IndexMap::new(),
            assign_ref: None,
            this: None,
            codegen: CodeGenerator::new(
                symbols.into_iter().chain([
                    ("println", do_println as *const u8),
                    ("println_fat", do_println_fat as *const u8),
                    ("println_bool", do_println_bool as *const u8),
                    ("println_addr", do_println_addr as *const u8),
                    ("println_str", do_println_str as *const u8),
                    ("println_float", do_println_f32 as *const u8),
                ]),
                settings::Flags::new(flag_builder),
            ),
            values: IndexMap::new(),
            vfuncs: IndexMap::new(),
            current_funcs: IndexMap::new(),
            globals: IndexMap::new(),
            funcs: IndexMap::new(),
            trait_to_vtable: IndexMap::new(),
            variables: IndexMap::new(),
            func_names: IndexMap::new(),
            comment_writer: CommentWriter::new(),
        };

        let ptr_type = compiler.codegen.module.isa().pointer_type();

        compiler.import_fn("println", [types::I64])?;
        compiler.import_fn("println_fat", [ptr_type, ptr_type])?;
        compiler.import_fn("println_str", [ptr_type, ptr_type])?;
        compiler.import_fn("println_bool", [types::I8])?;
        compiler.import_fn("println_addr", [types::I64])?;
        compiler.import_fn("println_float", [types::F32])?;

        let get_type_idx_id = {
            let mut ctx = compiler.codegen.module.make_context();

            ctx.func.signature.params.push(AbiParam::new(ptr_type));
            ctx.func.signature.params.push(AbiParam::new(ptr_type));
            ctx.func.signature.returns.push(AbiParam::new(ptr_type));

            let func = compiler.codegen.module.declare_function("get_type_idx", Linkage::Local, &ctx.func.signature)?;

            let mut fn_builder_ctx = FunctionBuilderContext::new();
            let mut fn_builder = FunctionBuilder::new(&mut ctx.func, &mut fn_builder_ctx);

            let entry_block = fn_builder.create_block();

            fn_builder.append_block_params_for_function_params(entry_block);
            fn_builder.switch_to_block(entry_block);
            fn_builder.seal_block(entry_block);

            let vtable_ptr = fn_builder.block_params(entry_block)[1];
            let type_idx = VTablePtr::get_type_idx(compiler.codegen.module.isa(), &mut fn_builder, vtable_ptr);

            fn_builder.ins().return_(&[type_idx]);

            compiler.codegen.module.define_function(func, &mut ctx)?;

            func
        };

        let get_size_id = {
            let mut ctx = compiler.codegen.module.make_context();

            ctx.func.signature.params.push(AbiParam::new(ptr_type));
            ctx.func.signature.params.push(AbiParam::new(ptr_type));
            ctx.func.signature.returns.push(AbiParam::new(ptr_type));

            let func = compiler.codegen.module.declare_function("get_size", Linkage::Local, &ctx.func.signature)?;

            let mut fn_builder_ctx = FunctionBuilderContext::new();
            let mut fn_builder = FunctionBuilder::new(&mut ctx.func, &mut fn_builder_ctx);

            let entry_block = fn_builder.create_block();

            fn_builder.append_block_params_for_function_params(entry_block);
            fn_builder.switch_to_block(entry_block);
            fn_builder.seal_block(entry_block);

            let size = fn_builder.block_params(entry_block)[1];

            fn_builder.ins().return_(&[size]);

            compiler.codegen.module.define_function(func, &mut ctx)?;

            func
        };

        compiler.func_names.insert(get_type_idx_id, "get_type_idx".to_owned());
        compiler.func_names.insert(get_size_id, "get_size".to_owned());

        compiler.globals.insert("get_type_idx".to_owned(), get_type_idx_id);
        compiler.globals.insert("get_size".to_owned(), get_size_id);

        Ok(compiler)
    }

    fn get<T: AsRef<str>>(&self, name: T) -> Option<ValueOrFunc> {
        self.values.get(name.as_ref()).cloned()
    }

    pub fn start_compiling(&mut self) -> FuncCompiler<'_> {
        FuncCompiler {
            ctx: self.codegen.module.make_context(),
            fn_builder_ctx: FunctionBuilderContext::new(),
            checker: TypeChecker::new(),
            compiler: self,
        }
    }
}

#[derive(Debug, Clone)]
enum ValueOrFunc {
    Value(ir::Value),
    Values(Vec<ir::Value>),
    FuncRef(ir::FuncRef),
    FatPtr(ir::Value, ir::Value),
    Nothing,
}

pub trait CompileTypedAST<T = ()> {
    /// # Errors
    ///
    /// Returns `CompileError` if compilation fails. This can happen in two
    /// cases: if the referenced variable is not found or if type inference
    /// fails.
    fn compile(self, ast: &TypedAST, checker: &TypeChecker, compiler: &mut Compiler, fn_builder: &mut FunctionBuilder) -> CompileResult<T>;
}

pub trait AsIrType {
    fn as_ir_type(&self, solver: &TypeSolver, isa: &dyn TargetIsa) -> MollieType;
}

impl AsIrType for TypeInfoRef {
    fn as_ir_type(&self, solver: &TypeSolver, isa: &dyn TargetIsa) -> MollieType {
        MollieType::Regular(match solver.get_info(*self) {
            TypeInfo::Unknown(fallback) => match fallback {
                Some(ty) => return ty.as_ir_type(solver, isa),
                None => ir::types::INVALID,
            },
            TypeInfo::Primitive(primitive_type) => match primitive_type {
                PrimitiveType::Any => unimplemented!(),
                PrimitiveType::ISize | PrimitiveType::USize => isa.pointer_type(),
                PrimitiveType::String | PrimitiveType::Component => {
                    return MollieType::Fat(isa.pointer_type(), isa.pointer_type());
                }
                PrimitiveType::I64 | PrimitiveType::U64 => ir::types::I64,
                PrimitiveType::I32 | PrimitiveType::U32 => ir::types::I32,
                PrimitiveType::I16 | PrimitiveType::U16 => ir::types::I16,
                PrimitiveType::I8 | PrimitiveType::U8 | PrimitiveType::Boolean => ir::types::I8,
                PrimitiveType::Float => ir::types::F32,
                PrimitiveType::Void => unimplemented!(),
                PrimitiveType::Null => unimplemented!(),
            },
            TypeInfo::Complex(_, ComplexTypeKind::Struct | ComplexTypeKind::Component, _) | TypeInfo::Func(..) => isa.pointer_type(),
            TypeInfo::Array(..) | TypeInfo::Trait(..) | TypeInfo::Complex(_, ComplexTypeKind::Enum, _) => {
                return MollieType::Fat(isa.pointer_type(), isa.pointer_type());
            }
            TypeInfo::Generic(..) | TypeInfo::Ref(_) => unreachable!(),
        })
    }
}

impl CompileTypedAST<ValueOrFunc> for StmtRef {
    fn compile(self, ast: &TypedAST, checker: &TypeChecker, compiler: &mut Compiler, fn_builder: &mut FunctionBuilder) -> CompileResult<ValueOrFunc> {
        match &ast[self] {
            Stmt::Expr(expr_ref) => expr_ref.compile(ast, checker, compiler, fn_builder),
            Stmt::VariableDecl { name, value } => {
                let ty = ast[*value].ty.as_ir_type(&checker.solver, compiler.codegen.module.isa());
                let value = value.compile(ast, checker, compiler, fn_builder)?;

                match (ty, value) {
                    (MollieType::Regular(ty), ValueOrFunc::Value(value)) => {
                        let var = fn_builder.declare_var(ty);

                        fn_builder.def_var(var, value);

                        compiler.variables.insert(name.clone(), Var::Regular(var));
                    }
                    (MollieType::Fat(ty, metadata_ty), ValueOrFunc::FatPtr(value, metadata)) => {
                        let var = fn_builder.declare_var(ty);
                        let metadata_var = fn_builder.declare_var(metadata_ty);

                        fn_builder.def_var(var, value);
                        fn_builder.def_var(metadata_var, metadata);

                        compiler.variables.insert(name.clone(), Var::Fat(var, metadata_var));
                    }
                    (ty, val) => panic!("can't create variable called {name} with type {ty:?} and value = {val:?}"),
                }

                Ok(ValueOrFunc::Nothing)
            }
            Stmt::Import(_) => todo!(),
        }
    }
}

impl CompileTypedAST<ValueOrFunc> for BlockRef {
    fn compile(self, ast: &TypedAST, checker: &TypeChecker, compiler: &mut Compiler, fn_builder: &mut FunctionBuilder) -> CompileResult<ValueOrFunc> {
        for statement in &ast[self].value.stmts {
            statement.compile(ast, checker, compiler, fn_builder)?;
        }

        Ok(if let Some(expr) = ast[self].value.expr {
            let value = expr.compile(ast, checker, compiler, fn_builder)?;
            let stmt = checker.solver.get_info2(ast[expr].ty);

            // if let TypeInfo::Complex(..) = stmt
            // && let ValueOrFunc::Value(ptr) = value
            // {
            // let mut values = Vec::new();
            //
            // let hash = checker.solver.hash_of(ast[expr].ty);
            //
            // for (field, _) in
            // compiler.complex_types[&hash].variants[ComplexTypeVariantRef::new(0)].fields.
            // values() { values.push(fn_builder.ins().load(field.ty,
            // ir::MemFlags::trusted(), ptr, field.offset)); }
            //
            // ValueOrFunc::Values(values)
            // } else
            if matches!(stmt, TypeInfo::Primitive(PrimitiveType::String))
                && let ValueOrFunc::Value(ptr) = value
            {
                let size_ty = compiler.codegen.module.isa().pointer_type();

                ValueOrFunc::Values(vec![
                    fn_builder.ins().load(size_ty, ir::MemFlags::trusted(), ptr, 0),
                    fn_builder.ins().load(size_ty, ir::MemFlags::trusted(), ptr, size_ty.bytes().cast_signed()),
                ])
            } else {
                value
            }
        } else {
            ValueOrFunc::Nothing
        })
    }
}

impl CompileTypedAST<ValueOrFunc> for ExprRef {
    fn compile(self, ast: &TypedAST, checker: &TypeChecker, compiler: &mut Compiler, fn_builder: &mut FunctionBuilder) -> CompileResult<ValueOrFunc> {
        match &ast[self].value {
            Expr::Literal(literal_expr) => Ok(ValueOrFunc::Value(match literal_expr {
                &LiteralExpr::Integer(value) => match ast[self].ty.as_ir_type(&checker.solver, compiler.codegen.module.isa()) {
                    MollieType::Regular(ty) => fn_builder.ins().iconst(ty, value),
                    MollieType::Fat(..) => return Ok(ValueOrFunc::Nothing),
                },
                &LiteralExpr::Float(value) => fn_builder.ins().f32const(value),
                LiteralExpr::String(value) => {
                    let len = value.len();

                    compiler.codegen.data_desc.define(value.bytes().collect());

                    let id = compiler.codegen.module.declare_anonymous_data(false, false).unwrap();

                    compiler.codegen.module.define_data(id, &compiler.codegen.data_desc).unwrap();
                    compiler.codegen.data_desc.clear();
                    compiler.codegen.module.finalize_definitions().unwrap();

                    let data_id = compiler.codegen.module.declare_data_in_func(id, fn_builder.func);

                    let ptr = fn_builder.ins().global_value(compiler.codegen.module.isa().pointer_type(), data_id);
                    let size = fn_builder.ins().iconst(compiler.codegen.module.isa().pointer_type(), len.cast_signed() as i64);

                    return Ok(ValueOrFunc::FatPtr(ptr, size));
                }
                &LiteralExpr::Boolean(value) => fn_builder.ins().iconst(ir::types::I8, i64::from(value)),
            })),
            &Expr::If { condition, block, else_block } => {
                if let ValueOrFunc::Value(cond_result) = condition.compile(ast, checker, compiler, fn_builder)? {
                    let then_block = fn_builder.create_block();
                    let after_block = fn_builder.create_block();

                    let returning_param = if let Some(final_stmt) = &ast[block].value.expr {
                        Some(match ast[*final_stmt].ty.as_ir_type(&checker.solver, compiler.codegen.module.isa()) {
                            MollieType::Fat(ty, metadata_ty) => ValueOrFunc::FatPtr(
                                fn_builder.append_block_param(after_block, ty),
                                fn_builder.append_block_param(after_block, metadata_ty),
                            ),
                            MollieType::Regular(ty) => ValueOrFunc::Value(fn_builder.append_block_param(after_block, ty)),
                        })
                    } else {
                        None
                    };

                    let returned = if let Some(otherwise) = else_block {
                        let else_block = fn_builder.create_block();

                        fn_builder.ins().brif(cond_result, then_block, &[], else_block, &[]);

                        fn_builder.switch_to_block(then_block);
                        fn_builder.seal_block(then_block);

                        let returned = block.compile(ast, checker, compiler, fn_builder)?;

                        match returned {
                            ValueOrFunc::Value(value) => fn_builder.ins().jump(after_block, &[ir::BlockArg::Value(value)]),
                            ValueOrFunc::Values(values) => fn_builder
                                .ins()
                                .jump(after_block, values.into_iter().map(ir::BlockArg::Value).collect::<Box<[_]>>().as_ref()),
                            ValueOrFunc::FuncRef(func_ref) => todo!(),
                            ValueOrFunc::FatPtr(value, metadata) => {
                                fn_builder.ins().jump(after_block, &[ir::BlockArg::Value(value), ir::BlockArg::Value(metadata)])
                            }
                            ValueOrFunc::Nothing => fn_builder.ins().jump(after_block, &[]),
                        };

                        fn_builder.switch_to_block(else_block);
                        fn_builder.seal_block(else_block);

                        otherwise.compile(ast, checker, compiler, fn_builder)?
                    } else {
                        fn_builder.ins().brif(cond_result, then_block, &[], after_block, &[]);

                        fn_builder.switch_to_block(then_block);
                        fn_builder.seal_block(then_block);

                        block.compile(ast, checker, compiler, fn_builder)?
                    };

                    match returned {
                        ValueOrFunc::Value(value) => fn_builder.ins().jump(after_block, &[ir::BlockArg::Value(value)]),
                        ValueOrFunc::Values(values) => fn_builder
                            .ins()
                            .jump(after_block, values.into_iter().map(ir::BlockArg::Value).collect::<Box<[_]>>().as_ref()),
                        ValueOrFunc::FuncRef(func_ref) => todo!(),
                        ValueOrFunc::FatPtr(value, metadata) => {
                            fn_builder.ins().jump(after_block, &[ir::BlockArg::Value(value), ir::BlockArg::Value(metadata)])
                        }
                        ValueOrFunc::Nothing => fn_builder.ins().jump(after_block, &[]),
                    };

                    fn_builder.switch_to_block(after_block);
                    fn_builder.seal_block(after_block);

                    returning_param.map_or(Ok(ValueOrFunc::Nothing), Ok)
                } else {
                    Ok(ValueOrFunc::Nothing)
                }
            }
            Expr::Block(block_ref) => block_ref.compile(ast, checker, compiler, fn_builder),
            Expr::Var(name) => {
                if let Some((_, value)) = compiler.assign_ref.take_if(|(lhs_ref, _)| *lhs_ref == self) {
                    let value = value.compile(ast, checker, compiler, fn_builder)?;

                    match (compiler.variables.get(name), value) {
                        (Some(Var::Regular(v)), ValueOrFunc::Value(value)) => {
                            fn_builder.def_var(*v, value);

                            Ok(ValueOrFunc::Nothing)
                        }
                        (Some(Var::Fat(v, m)), ValueOrFunc::FatPtr(value, metadata)) => {
                            fn_builder.def_var(*v, value);
                            fn_builder.def_var(*m, metadata);

                            Ok(ValueOrFunc::Nothing)
                        }
                        _ => unimplemented!(),
                    }
                } else if let Some(v) = compiler.get(name) {
                    Ok(v)
                } else if let Some(&func) = compiler.globals.get(name) {
                    let func = compiler.codegen.module.declare_func_in_func(func, fn_builder.func);

                    compiler.values.insert(name.clone(), ValueOrFunc::FuncRef(func));

                    Ok(ValueOrFunc::FuncRef(func))
                } else if let Some(v) = compiler.variables.get(name) {
                    match *v {
                        Var::Regular(v) => Ok(ValueOrFunc::Value(fn_builder.use_var(v))),
                        Var::Fat(v, m) => Ok(ValueOrFunc::FatPtr(fn_builder.use_var(v), fn_builder.use_var(m))),
                    }
                } else {
                    unimplemented!("there's no anything for {name}")
                }
            }
            Expr::Access { target, field } => {
                let v = target.compile(ast, checker, compiler, fn_builder)?;

                if let ValueOrFunc::Value(v) = v {
                    if let TypeInfo::Complex(ty, ..) = checker.solver.get_info2(ast[*target].ty) {
                        let hash = checker.solver.hash_of(ast[*target].ty);
                        let assign_value = match compiler.assign_ref.take_if(|(lhs_ref, _)| *lhs_ref == self) {
                            Some((_, assign)) => Some(assign.compile(ast, checker, compiler, fn_builder)?),
                            None => None,
                        };

                        let (field, _) = &compiler.complex_types[&hash].variants[ComplexTypeVariantRef::new(0)].fields[*field];

                        Ok(match (field.ty, assign_value) {
                            (MollieType::Regular(ty), None) => ValueOrFunc::Value(fn_builder.ins().load(ty, ir::MemFlags::trusted(), v, field.offset)),
                            (MollieType::Fat(ty, metadata_ty), None) => ValueOrFunc::FatPtr(
                                fn_builder.ins().load(ty, ir::MemFlags::trusted(), v, field.offset),
                                fn_builder
                                    .ins()
                                    .load(metadata_ty, ir::MemFlags::trusted(), v, field.offset + ty.bytes().cast_signed()),
                            ),
                            (MollieType::Regular(_), Some(ValueOrFunc::Value(value))) => {
                                fn_builder.ins().store(ir::MemFlags::trusted(), value, v, field.offset);

                                ValueOrFunc::Nothing
                            }
                            (MollieType::Fat(ty, _), Some(ValueOrFunc::FatPtr(value, metadata))) => {
                                fn_builder.ins().store(ir::MemFlags::trusted(), value, v, field.offset);
                                fn_builder
                                    .ins()
                                    .store(ir::MemFlags::trusted(), metadata, v, field.offset + ty.bytes().cast_signed());

                                ValueOrFunc::Nothing
                            }
                            _ => unimplemented!(),
                        })
                    } else {
                        unimplemented!()
                    }
                } else {
                    unimplemented!()
                }
            }
            &Expr::VTableAccess { target, func } => {
                let target_val = target.compile(ast, checker, compiler, fn_builder)?;

                match func {
                    VFunc::Known(vtable_ref, vfunc_ref) => {
                        compiler.this.replace(target_val);

                        let target_hash = checker.solver.hash_of(ast[target].ty);

                        return Ok(ValueOrFunc::FuncRef(compiler.try_import_vfunc(target_hash, vtable_ref, vfunc_ref, fn_builder)));
                    }
                    VFunc::Unknown(_, trait_func_ref) => {
                        if let ValueOrFunc::FatPtr(value, vtable_ptr) = target_val {
                            let vtable_func =
                                VTablePtr::get_func_ptr(compiler.codegen.module.isa(), fn_builder, vtable_ptr, trait_func_ref.index().try_into()?);

                            compiler.this.replace(ValueOrFunc::Value(value));

                            return Ok(ValueOrFunc::Value(vtable_func));
                        }
                    }
                }

                Ok(ValueOrFunc::Nothing)
            }
            // target[index]
            &Expr::Index { target, index } => {
                let target = target.compile(ast, checker, compiler, fn_builder)?;
                let index = index.compile(ast, checker, compiler, fn_builder)?;

                if let (&ValueOrFunc::FatPtr(ptr, _), &ValueOrFunc::Value(index)) = (&target, &index) {
                    let element_type = ast[self].ty.as_ir_type(&checker.solver, compiler.codegen.module.isa());
                    let size = match element_type {
                        MollieType::Fat(ty, metadata_ty) => ty.bytes() + metadata_ty.bytes(),
                        MollieType::Regular(ty) => ty.bytes(),
                    };

                    let size = fn_builder.ins().iconst(compiler.codegen.module.isa().pointer_type(), i64::from(size));
                    let offset = fn_builder.ins().imul(size, index);
                    let ptr = fn_builder.ins().iadd(ptr, offset);

                    if let Some((_, assign)) = compiler.assign_ref.take_if(|(lhs_ref, _)| *lhs_ref == self) {
                        if let ValueOrFunc::Value(value) = assign.compile(ast, checker, compiler, fn_builder)? {
                            fn_builder.ins().store(ir::MemFlags::trusted(), value, ptr, 0);

                            return Ok(ValueOrFunc::Nothing);
                        }
                    } else {
                        return match element_type {
                            MollieType::Fat(ty, metadata_ty) => Ok(ValueOrFunc::FatPtr(
                                fn_builder.ins().load(ty, ir::MemFlags::trusted(), ptr, 0),
                                fn_builder.ins().load(metadata_ty, ir::MemFlags::trusted(), ptr, ty.bytes().cast_signed()),
                            )),
                            MollieType::Regular(ty) => Ok(ValueOrFunc::Value(fn_builder.ins().load(ty, ir::MemFlags::trusted(), ptr, 0))),
                        };
                    }
                }

                todo!("no target[index] for {target:?}[{index:?}]")
            }
            Expr::While { condition, block } => {
                // compiler.push_frame();

                let condition_block = fn_builder.create_block();
                let inner_block = fn_builder.create_block();
                let after_block = fn_builder.create_block();

                fn_builder.ins().jump(condition_block, &[]);
                fn_builder.switch_to_block(condition_block);

                if let ValueOrFunc::Value(value) = condition.compile(ast, checker, compiler, fn_builder)? {
                    fn_builder.ins().brif(value, inner_block, &[], after_block, &[]);
                }

                fn_builder.switch_to_block(inner_block);
                fn_builder.seal_block(inner_block);

                let value = block.compile(ast, checker, compiler, fn_builder)?;

                fn_builder.ins().jump(condition_block, &[]);
                fn_builder.switch_to_block(after_block);
                fn_builder.seal_block(condition_block);
                fn_builder.seal_block(after_block);

                // compiler.pop_frame();

                Ok(value)
            }
            Expr::Array(expr_refs) => {
                if let &TypeInfo::Array(element, _) = checker.solver.get_info(ast[self].ty) {
                    let mut values = Vec::with_capacity(expr_refs.len());

                    for expr_ref in expr_refs {
                        if let ValueOrFunc::Value(value) = expr_ref.compile(ast, checker, compiler, fn_builder)? {
                            if checker.solver.get_info(element).is_any_component() {
                                let hash = checker.solver.hash_of(ast[*expr_ref].ty);
                                let metadata = fn_builder.ins().iconst(compiler.codegen.module.isa().pointer_type(), hash.cast_signed());

                                values.push(value);
                                values.push(metadata);
                            } else if let &TypeInfo::Trait(t, _) = checker.solver.get_info(element) {
                                let hash = checker.solver.hash_of(ast[*expr_ref].ty);
                                let data_id = compiler
                                    .codegen
                                    .module
                                    .declare_data_in_func(compiler.trait_to_vtable[&(hash, Some(t))], fn_builder.func);

                                let metadata = fn_builder.ins().global_value(compiler.codegen.module.isa().pointer_type(), data_id);

                                values.push(value);
                                values.push(metadata);
                            } else {
                                values.push(value);
                            }
                        }
                    }

                    let size = fn_builder.ins().iconst(compiler.codegen.module.isa().pointer_type(), expr_refs.len() as i64);

                    let ir_element = ast[self].ty.as_ir_type(&checker.solver, compiler.codegen.module.isa());

                    let arr = Array { element: ir_element };

                    compiler.codegen.data_desc.define_zeroinit(arr.get_size(values.len()) as usize);

                    let id = compiler.codegen.module.declare_anonymous_data(true, false).unwrap();

                    compiler.codegen.module.define_data(id, &compiler.codegen.data_desc).unwrap();
                    compiler.codegen.data_desc.clear();

                    let data_id = compiler.codegen.module.declare_data_in_func(id, fn_builder.func);
                    let ptr = fn_builder.ins().global_value(compiler.codegen.module.isa().pointer_type(), data_id);

                    match ir_element {
                        MollieType::Regular(_) => {
                            for (index, value) in values.into_iter().enumerate() {
                                compiler.comment_writer.add_post_comment(
                                    fn_builder.ins().store(ir::MemFlags::trusted(), value, ptr, arr.get_offset_of(index)),
                                    format!("array element #{index}"),
                                );
                            }
                        }
                        MollieType::Fat(ty, _) => {
                            for (index, (value, metadata)) in values.into_iter().tuples::<(ir::Value, ir::Value)>().enumerate() {
                                compiler.comment_writer.add_post_comment(
                                    fn_builder.ins().store(ir::MemFlags::trusted(), value, ptr, arr.get_offset_of(index)),
                                    format!("array element #{index} (value)"),
                                );

                                compiler.comment_writer.add_post_comment(
                                    fn_builder
                                        .ins()
                                        .store(ir::MemFlags::trusted(), metadata, ptr, arr.get_offset_of(index) + ty.bytes().cast_signed()),
                                    format!("array element #{index} (metadata)"),
                                );
                            }
                        }
                    }

                    Ok(ValueOrFunc::FatPtr(ptr, size))
                } else {
                    Ok(ValueOrFunc::Nothing)
                }
            }
            &Expr::Binary {
                operator,
                lhs: lhs_ref,
                rhs: rhs_ref,
            } => {
                if matches!(operator, Operator::Assign) {
                    let old = compiler.assign_ref.replace((lhs_ref, rhs_ref));

                    let lhs = lhs_ref.compile(ast, checker, compiler, fn_builder)?;

                    if old.is_some() {
                        compiler.assign_ref = old;
                    } else if compiler.assign_ref.is_some() {
                        compiler.assign_ref.take();
                    }

                    Ok(lhs)
                } else {
                    let lhs = lhs_ref.compile(ast, checker, compiler, fn_builder)?;
                    let rhs = rhs_ref.compile(ast, checker, compiler, fn_builder)?;

                    let lhs_ty = checker.solver.get_info2(ast[lhs_ref].ty);
                    let rhs_ty = checker.solver.get_info2(ast[rhs_ref].ty);

                    if let (&ValueOrFunc::Value(lhs), &ValueOrFunc::Value(rhs)) = (&lhs, &rhs) {
                        if lhs_ty.is_unsigned_integer() && rhs_ty.is_unsigned_integer() {
                            Ok(ValueOrFunc::Value(match operator {
                                Operator::Add => fn_builder.ins().uadd_overflow(lhs, rhs).0,
                                Operator::Sub => fn_builder.ins().usub_overflow(lhs, rhs).0,
                                Operator::Mul => fn_builder.ins().umul_overflow(lhs, rhs).0,
                                Operator::Div => fn_builder.ins().udiv(lhs, rhs),
                                Operator::Equal => fn_builder.ins().icmp(IntCC::Equal, lhs, rhs),
                                Operator::NotEqual => fn_builder.ins().icmp(IntCC::NotEqual, lhs, rhs),
                                Operator::LessThan => fn_builder.ins().icmp(IntCC::UnsignedLessThan, lhs, rhs),
                                Operator::GreaterThan => fn_builder.ins().icmp(IntCC::UnsignedGreaterThan, lhs, rhs),
                                _ => unreachable!(),
                            }))
                        } else if lhs_ty.is_signed_integer() && rhs_ty.is_signed_integer() {
                            Ok(ValueOrFunc::Value(match operator {
                                Operator::Add => fn_builder.ins().iadd(lhs, rhs),
                                Operator::Sub => fn_builder.ins().isub(lhs, rhs),
                                Operator::Mul => fn_builder.ins().imul(lhs, rhs),
                                Operator::Div => fn_builder.ins().sdiv(lhs, rhs),
                                Operator::Equal => fn_builder.ins().icmp(IntCC::Equal, lhs, rhs),
                                Operator::NotEqual => fn_builder.ins().icmp(IntCC::NotEqual, lhs, rhs),
                                Operator::LessThan => fn_builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs),
                                Operator::GreaterThan => fn_builder.ins().icmp(IntCC::SignedGreaterThan, lhs, rhs),
                                _ => unreachable!(),
                            }))
                        } else if matches!(lhs_ty, TypeInfo::Primitive(PrimitiveType::Float)) && matches!(rhs_ty, TypeInfo::Primitive(PrimitiveType::Float)) {
                            Ok(ValueOrFunc::Value(match operator {
                                Operator::Add => fn_builder.ins().fadd(lhs, rhs),
                                Operator::Sub => fn_builder.ins().fsub(lhs, rhs),
                                Operator::Mul => fn_builder.ins().fmul(lhs, rhs),
                                Operator::Div => fn_builder.ins().fdiv(lhs, rhs),
                                Operator::Equal => fn_builder.ins().fcmp(FloatCC::Equal, lhs, rhs),
                                Operator::NotEqual => fn_builder.ins().fcmp(FloatCC::NotEqual, lhs, rhs),
                                Operator::LessThan => fn_builder.ins().fcmp(FloatCC::LessThan, lhs, rhs),
                                Operator::GreaterThan => fn_builder.ins().fcmp(FloatCC::GreaterThan, lhs, rhs),
                                _ => unreachable!(),
                            }))
                        } else {
                            Ok(ValueOrFunc::Value(match operator {
                                Operator::Add => fn_builder.ins().iadd(lhs, rhs),
                                Operator::Sub => fn_builder.ins().isub(lhs, rhs),
                                Operator::Mul => fn_builder.ins().imul(lhs, rhs),
                                Operator::Div => fn_builder.ins().udiv(lhs, rhs),
                                Operator::Equal => fn_builder.ins().icmp(IntCC::Equal, lhs, rhs),
                                Operator::NotEqual => fn_builder.ins().icmp(IntCC::NotEqual, lhs, rhs),
                                Operator::LessThan => fn_builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs),
                                Operator::GreaterThan => fn_builder.ins().icmp(IntCC::SignedGreaterThan, lhs, rhs),
                                _ => unreachable!(),
                            }))
                        }
                    } else {
                        unimplemented!("{lhs:?} {operator} {rhs:?}")
                    }
                }
            }
            Expr::Call { func, args } => {
                if let Some(TypeInfo::Func(arg_types, returns)) = checker.solver.get_maybe_info(ast[*func].ty) {
                    let v = func.compile(ast, checker, compiler, fn_builder)?;
                    let mut arg_values = Vec::new();

                    if let Some(this) = compiler.this.take() {
                        match this {
                            ValueOrFunc::Value(value) => arg_values.push(value),
                            ValueOrFunc::FatPtr(value, metadata) => {
                                arg_values.push(value);
                                arg_values.push(metadata);
                            }
                            ValueOrFunc::Nothing => (),
                            _ => panic!("received incorrect value for <self> argument: {this:?}"),
                        }
                    }

                    for arg in args {
                        let value = arg.compile(ast, checker, compiler, fn_builder)?;

                        match value {
                            ValueOrFunc::Value(value) => arg_values.push(value),
                            ValueOrFunc::FatPtr(value, metadata) => {
                                arg_values.push(value);
                                arg_values.push(metadata);
                            }
                            ValueOrFunc::Nothing => (),
                            _ => panic!("received incorrect value for argument: {arg:?} {value:?}"),
                        }
                    }

                    let value = if let ValueOrFunc::FuncRef(func) = v {
                        let result = fn_builder.ins().call(func, &arg_values);

                        match fn_builder.inst_results(result) {
                            [] => ValueOrFunc::Nothing,
                            &[value] => ValueOrFunc::Value(value),
                            &[value, metadata]
                                if returns != &checker.core_types.void && returns.as_ir_type(&checker.solver, compiler.codegen.module.isa()).is_fat() =>
                            {
                                ValueOrFunc::FatPtr(value, metadata)
                            }
                            values => ValueOrFunc::Values(values.to_vec()),
                        }
                    } else if let ValueOrFunc::Value(func_addr) = v {
                        let mut signature = compiler.codegen.module.make_signature();

                        for arg in arg_types {
                            if arg.as_inner() != &checker.core_types.void {
                                if arg.is_this() && checker.solver.get_info(arg.inner()).is_trait() {
                                    signature.params.push(ir::AbiParam::new(compiler.codegen.module.isa().pointer_type()));
                                } else {
                                    arg.as_inner()
                                        .as_ir_type(&checker.solver, compiler.codegen.module.isa())
                                        .add_to_params(&mut signature.params);
                                }
                            }
                        }

                        if returns != &checker.core_types.void {
                            returns
                                .as_ir_type(&checker.solver, compiler.codegen.module.isa())
                                .add_to_params(&mut signature.returns);
                        }

                        let sig = fn_builder.import_signature(signature);
                        let result = fn_builder.ins().call_indirect(sig, func_addr, &arg_values);

                        match fn_builder.inst_results(result) {
                            [] => ValueOrFunc::Nothing,
                            &[value] => ValueOrFunc::Value(value),
                            &[value, metadata]
                                if returns != &checker.core_types.void && returns.as_ir_type(&checker.solver, compiler.codegen.module.isa()).is_fat() =>
                            {
                                ValueOrFunc::FatPtr(value, metadata)
                            }
                            values => ValueOrFunc::Values(values.to_vec()),
                        }
                    } else {
                        ValueOrFunc::Nothing
                    };

                    Ok(value)
                } else {
                    panic!("dada");
                }
            }
            Expr::Closure { args, body } => {
                let values = mem::take(&mut compiler.values);
                let vfuncs = std::mem::take(&mut compiler.vfuncs);
                let current_funcs = std::mem::take(&mut compiler.current_funcs);

                let mut signature = compiler.codegen.module.make_signature();

                if let TypeInfo::Func(args, returns) = checker.solver.get_info(ast[self].ty) {
                    for arg in args {
                        if arg.as_inner() != &checker.core_types.void {
                            arg.as_inner()
                                .as_ir_type(&checker.solver, compiler.codegen.module.isa())
                                .add_to_params(&mut signature.params);
                        }
                    }

                    if returns != &checker.core_types.void {
                        returns
                            .as_ir_type(&checker.solver, compiler.codegen.module.isa())
                            .add_to_params(&mut signature.returns);
                    }
                }

                let mut ctx = compiler.codegen.module.make_context();
                let func_id = compiler.codegen.module.declare_anonymous_function(&signature).unwrap();

                ctx.func.signature = signature.clone();

                let mut fn_builder_ctx = FunctionBuilderContext::new();
                let mut closure_fn_builder = FunctionBuilder::new(&mut ctx.func, &mut fn_builder_ctx);

                let entry_block = closure_fn_builder.create_block();

                closure_fn_builder.append_block_params_for_function_params(entry_block);
                closure_fn_builder.switch_to_block(entry_block);
                closure_fn_builder.seal_block(entry_block);

                let mut index = 0;

                for (arg, name) in args.iter().enumerate() {
                    let value = closure_fn_builder.block_params(entry_block)[index];
                    let ty = closure_fn_builder.func.signature.params[index].value_type;

                    let var = closure_fn_builder.declare_var(ty);

                    closure_fn_builder.def_var(var, value);

                    if let TypeInfo::Func(args, _) = checker.solver.get_info(ast[self].ty)
                        && args[arg].as_inner().as_ir_type(&checker.solver, compiler.codegen.module.isa()).is_fat()
                    {
                        let value = closure_fn_builder.block_params(entry_block)[index + 1];
                        let ty = closure_fn_builder.func.signature.params[index + 1].value_type;

                        let metadata_var = closure_fn_builder.declare_var(ty);

                        closure_fn_builder.def_var(metadata_var, value);

                        compiler.variables.insert(name.clone(), Var::Fat(var, metadata_var));

                        index += 2;
                    } else {
                        compiler.variables.insert(name.clone(), Var::Regular(var));

                        index += 1;
                    }
                }

                let result = body.compile(ast, checker, compiler, &mut closure_fn_builder).unwrap();

                match result {
                    ValueOrFunc::Value(value) => closure_fn_builder.ins().return_(&[value]),
                    ValueOrFunc::Values(values) => closure_fn_builder.ins().return_(&values),
                    ValueOrFunc::FuncRef(func_ref) => {
                        let func_addr = closure_fn_builder.ins().func_addr(compiler.codegen.module.isa().pointer_type(), func_ref);

                        closure_fn_builder.ins().return_(&[func_addr])
                    }
                    ValueOrFunc::FatPtr(ptr, metadata) => closure_fn_builder.ins().return_(&[ptr, metadata]),
                    ValueOrFunc::Nothing => closure_fn_builder.ins().return_(&[]),
                };

                compiler.codegen.module.define_function(func_id, &mut ctx).unwrap();
                compiler.codegen.module.clear_context(&mut ctx);
                compiler.codegen.module.finalize_definitions().unwrap();

                for name in args {
                    compiler.variables.shift_remove(name);
                }

                compiler.values = values;
                compiler.vfuncs = vfuncs;
                compiler.current_funcs = current_funcs;

                let func = compiler.codegen.module.declare_func_in_func(func_id, fn_builder.func);
                let ptr = fn_builder.ins().func_addr(compiler.codegen.module.isa().pointer_type(), func);

                Ok(ValueOrFunc::Value(ptr))
            }
            Expr::Construct { ty, fields } => {
                if checker.solver.get_info2(*ty).is_struct_like() {
                    let mut values = Vec::new();
                    let hash = checker.solver.hash_of(*ty);
                    let &TypeInfo::Complex(complex_ty, ..) = checker.solver.get_info2(*ty) else {
                        unreachable!()
                    };

                    for (field_ref, (field, _)) in compiler.complex_types[&hash].main_variant().fields.clone().into_iter() {
                        if let Some(property) = fields.iter().find(|property| property.0 == field_ref) {
                            let (got_ty, got) = match property.2 {
                                Some(expr) => (Some(ast[expr].ty), expr.compile(ast, checker, compiler, fn_builder)?),
                                None => match &field.default_value {
                                    Some(value) => (
                                        None,
                                        compile_constant(value, &mut compiler.codegen.module, &mut compiler.codegen.data_desc, fn_builder)
                                            .map_or(ValueOrFunc::Nothing, ValueOrFunc::Value),
                                    ),
                                    None => panic!("can't compile {field:?}: no value"),
                                },
                            };

                            if let &ValueOrFunc::Value(v) = &got {
                                if let MollieType::Regular(ty) = field.ty {
                                    assert_eq!(ty, fn_builder.func.dfg.value_type(v), "got incorrect type for ...");
                                    // if let TypeVariant::Trait(trait_index) = ty.variant {
                                    //     let vtable =
                                    // compiler.vtables[compiler.get_vtable_index(&got.variant).unwrap()][&
                                    // Some(trait_index)].0;     let data_id =
                                    // compiler.jit.module.declare_data_in_func(vtable, fn_builder.func);
                                    //     let vtable =
                                    // fn_builder.ins().global_value(compiler.jit.module.isa().pointer_type(),
                                    // data_id);

                                    //     values.push(FatPtr::new(compiler.jit.module.isa(), fn_builder, v,
                                    // vtable)); } else {
                                    values.push(v);
                                    // }
                                } else if let MollieType::Fat(ty, meta) = field.ty
                                    && let Some(got_ty) = got_ty
                                {
                                    assert_eq!(ty, fn_builder.func.dfg.value_type(v), "got incorrect type for ...");

                                    let hash = checker.solver.hash_of(got_ty);
                                    let metadata = fn_builder.ins().iconst(meta, hash.cast_signed());

                                    values.push(v);
                                    values.push(metadata);
                                }
                            } else if let &ValueOrFunc::FatPtr(v, m) = &got
                                && let MollieType::Fat(ty, metadata_ty) = field.ty
                            {
                                assert_eq!(ty, fn_builder.func.dfg.value_type(v), "got incorrect type for ...");
                                assert_eq!(metadata_ty, fn_builder.func.dfg.value_type(m), "got incorrect type for ...");

                                values.push(v);
                                values.push(m);
                            }
                        }
                    }

                    return Ok(ValueOrFunc::Value({
                        compiler.codegen.data_desc.define_zeroinit(compiler.complex_types[&hash].size as usize);

                        let id = compiler.codegen.module.declare_anonymous_data(true, false).unwrap();

                        compiler.codegen.module.define_data(id, &compiler.codegen.data_desc).unwrap();
                        compiler.codegen.data_desc.clear();

                        let data_id = compiler.codegen.module.declare_data_in_func(id, fn_builder.func);
                        let ptr = fn_builder.ins().global_value(compiler.codegen.module.isa().pointer_type(), data_id);

                        let mut values = values.into_iter();

                        for (field_ref, (field, _)) in compiler.complex_types[&hash].variants[ComplexTypeVariantRef::ZERO].fields.iter() {
                            if let MollieType::Fat(ty, _) = field.ty
                                && let Some(value) = values.next()
                                && let Some(metadata) = values.next()
                            {
                                let name = &checker.complex_types[complex_ty].variants[ComplexTypeVariantRef::ZERO].fields[field_ref].0;

                                compiler.comment_writer.add_post_comment(
                                    fn_builder.ins().store(ir::MemFlags::trusted(), value, ptr, field.offset),
                                    format!(".{name} (value)"),
                                );

                                compiler.comment_writer.add_post_comment(
                                    fn_builder
                                        .ins()
                                        .store(ir::MemFlags::trusted(), metadata, ptr, field.offset + ty.bytes().cast_signed()),
                                    format!(".{name} (metadata)"),
                                );
                            } else if let Some(value) = values.next() {
                                let comment = format!(
                                    ".{}",
                                    checker.complex_types[complex_ty].variants[ComplexTypeVariantRef::ZERO].fields[field_ref].0
                                );

                                compiler
                                    .comment_writer
                                    .add_post_comment(fn_builder.ins().store(ir::MemFlags::trusted(), value, ptr, field.offset), comment);
                            }
                        }

                        ptr
                    }));
                }

                todo!()
            }
            Expr::ConstructEnum { ty, variant, fields } => {
                if checker.solver.get_info2(*ty).is_enum() {
                    let hash = checker.solver.hash_of(*ty);
                    let mut values = Vec::new();

                    if let Some(fields) = fields {
                        for (field_ref, (field, _)) in compiler.complex_types[&hash].variants[ComplexTypeVariantRef::new(*variant)]
                            .fields
                            .clone()
                            .into_iter()
                        {
                            if let Some(property) = fields.iter().find(|property| property.0 == field_ref) {
                                let got = match property.2 {
                                    Some(expr) => expr.compile(ast, checker, compiler, fn_builder)?,
                                    None => match &field.default_value {
                                        Some(value) => compile_constant(value, &mut compiler.codegen.module, &mut compiler.codegen.data_desc, fn_builder)
                                            .map_or(ValueOrFunc::Nothing, ValueOrFunc::Value),
                                        None => panic!("can't compile {field:?}: no value"),
                                    },
                                };

                                if let &ValueOrFunc::Value(v) = &got
                                    && let MollieType::Regular(ty) = field.ty
                                {
                                    assert_eq!(ty, fn_builder.func.dfg.value_type(v), "got incorrect type for ...");

                                    // if let TypeVariant::Trait(trait_index) = ty.variant {
                                    //     let vtable =
                                    // compiler.vtables[compiler.get_vtable_index(&got.variant).unwrap()][&
                                    // Some(trait_index)].0;     let data_id =
                                    // compiler.jit.module.declare_data_in_func(vtable, fn_builder.func);
                                    //     let vtable =
                                    // fn_builder.ins().global_value(compiler.jit.module.isa().pointer_type(),
                                    // data_id);

                                    //     values.push(FatPtr::new(compiler.jit.module.isa(), fn_builder, v,
                                    // vtable)); } else {
                                    values.push(v);
                                    // }
                                } else if let &ValueOrFunc::FatPtr(v, m) = &got
                                    && let MollieType::Fat(ty, metadata_ty) = field.ty
                                {
                                    assert_eq!(ty, fn_builder.func.dfg.value_type(v), "got incorrect type for ...");
                                    assert_eq!(metadata_ty, fn_builder.func.dfg.value_type(m), "got incorrect type for ...");

                                    values.push(v);
                                    values.push(m);
                                }
                            }
                        }
                    }

                    let metadata = fn_builder.ins().iconst(compiler.codegen.module.isa().pointer_type(), *variant as i64);
                    let enum_value = if compiler.complex_types[&hash].size == 0 {
                        metadata
                    } else {
                        compiler.complex_types[&hash]
                            .instance(ComplexTypeVariantRef::new(*variant), &mut compiler.codegen, fn_builder, values)
                            .unwrap()
                    };

                    return Ok(ValueOrFunc::FatPtr(enum_value, metadata));
                }

                Ok(ValueOrFunc::Nothing)
            }
            Expr::ConstructComponent { .. } => todo!(),
            Expr::IsPattern { target, pattern } => {
                fn compile_pattern(
                    target: &ValueOrFunc,
                    target_ty: TypeInfoRef,
                    pattern: &IsPattern,
                    ast: &TypedAST,
                    checker: &TypeChecker,
                    compiler: &mut Compiler,
                    fn_builder: &mut FunctionBuilder,
                ) -> CompileResult<ValueOrFunc> {
                    match pattern {
                        &IsPattern::Literal(expr_ref) => {
                            if let &ValueOrFunc::Value(target) = target {
                                let target_ty = checker.solver.get_info(target_ty);
                                let expr_ty = checker.solver.get_info(ast[expr_ref].ty);
                                let ValueOrFunc::Value(value) = expr_ref.compile(ast, checker, compiler, fn_builder)? else {
                                    unreachable!()
                                };

                                Ok(ValueOrFunc::Value(if target_ty.is_float() && expr_ty.is_float() {
                                    fn_builder.ins().fcmp(FloatCC::Equal, target, value)
                                } else {
                                    fn_builder.ins().icmp(IntCC::Equal, target, value)
                                }))
                            } else {
                                Ok(ValueOrFunc::Nothing)
                            }
                        }
                        IsPattern::EnumVariant {
                            target: target_ty,
                            target_args,
                            variant,
                            values,
                        } => {
                            if let &ValueOrFunc::FatPtr(target, metadata) = target {
                                let expected = fn_builder.ins().iconst(compiler.codegen().module.isa().pointer_type(), variant.index() as i64);
                                let result = fn_builder.ins().icmp(IntCC::Equal, metadata, expected);

                                let hash = {
                                    let mut state = DefaultHasher::new();

                                    "complex".hash(&mut state);

                                    target_ty.hash(&mut state);
                                    checker.complex_types[*target_ty].kind.hash(&mut state);

                                    for &arg in target_args {
                                        checker.solver.hash_into(&mut state, arg);
                                    }

                                    state.finish()
                                };

                                let declaration_block = fn_builder.create_block();
                                let after_block = fn_builder.create_block();

                                fn_builder.ins().brif(result, declaration_block, &[], after_block, &[]);

                                fn_builder.switch_to_block(declaration_block);
                                fn_builder.seal_block(declaration_block);

                                for (field_ref, name, pattern) in values {
                                    let (field, field_ty) = &compiler.complex_types[&hash].variants[*variant].fields[*field_ref];

                                    match field.ty {
                                        MollieType::Regular(ty) => {
                                            let value = fn_builder.ins().load(ty, ir::MemFlags::trusted(), target, field.offset);

                                            if let Some(pattern) = pattern {
                                                compile_pattern(&ValueOrFunc::Value(value), *field_ty, pattern, ast, checker, compiler, fn_builder)?;
                                            } else {
                                                let var = fn_builder.declare_var(ty);

                                                fn_builder.def_var(var, value);

                                                compiler.variables.insert(name.into(), Var::Regular(var));
                                            }
                                        }
                                        MollieType::Fat(ty, metadata_ty) => {
                                            let value = fn_builder.ins().load(ty, ir::MemFlags::trusted(), target, field.offset);
                                            let metadata =
                                                fn_builder
                                                    .ins()
                                                    .load(metadata_ty, ir::MemFlags::trusted(), target, field.offset + ty.bytes().cast_signed());

                                            if let Some(pattern) = pattern {
                                                compile_pattern(&ValueOrFunc::FatPtr(value, metadata), *field_ty, pattern, ast, checker, compiler, fn_builder)?;
                                            } else {
                                                let var = fn_builder.declare_var(ty);
                                                let metadata_var = fn_builder.declare_var(metadata_ty);

                                                fn_builder.def_var(var, value);
                                                fn_builder.def_var(metadata_var, metadata);

                                                compiler.variables.insert(name.into(), Var::Fat(var, metadata_var));
                                            }
                                        }
                                    }
                                }

                                fn_builder.ins().jump(after_block, &[]);

                                fn_builder.switch_to_block(after_block);
                                fn_builder.seal_block(after_block);

                                Ok(ValueOrFunc::Value(result))
                            } else {
                                Ok(ValueOrFunc::Nothing)
                            }
                        }
                        IsPattern::TypeName { ty, name } => {
                            if let &ValueOrFunc::FatPtr(ptr, metadata) = target {
                                let ptr_type = compiler.codegen.module.isa().pointer_type();
                                let expected_hash = checker.solver.hash_of(*ty);

                                let value = fn_builder.ins().iconst(ptr_type, expected_hash.cast_signed());

                                if checker.solver.get_info(target_ty).is_trait() {
                                    let metadata = fn_builder.ins().load(ptr_type, ir::MemFlags::trusted(), metadata, 0);
                                    let result = fn_builder.ins().icmp(IntCC::Equal, metadata, value);

                                    let declaration_block = fn_builder.create_block();
                                    let after_block = fn_builder.create_block();

                                    fn_builder.ins().brif(result, declaration_block, &[], after_block, &[]);

                                    fn_builder.switch_to_block(declaration_block);
                                    fn_builder.seal_block(declaration_block);

                                    let var = fn_builder.declare_var(ptr_type);

                                    fn_builder.def_var(var, ptr);

                                    compiler.variables.insert(name.into(), Var::Regular(var));

                                    fn_builder.ins().jump(after_block, &[]);

                                    fn_builder.switch_to_block(after_block);
                                    fn_builder.seal_block(after_block);

                                    Ok(ValueOrFunc::Value(result))
                                } else {
                                    let result = fn_builder.ins().icmp(IntCC::Equal, metadata, value);

                                    let declaration_block = fn_builder.create_block();
                                    let after_block = fn_builder.create_block();

                                    fn_builder.ins().brif(result, declaration_block, &[], after_block, &[]);

                                    fn_builder.switch_to_block(declaration_block);
                                    fn_builder.seal_block(declaration_block);

                                    let var = fn_builder.declare_var(ptr_type);

                                    fn_builder.def_var(var, ptr);

                                    compiler.variables.insert(name.into(), Var::Regular(var));

                                    fn_builder.ins().jump(after_block, &[]);

                                    fn_builder.switch_to_block(after_block);
                                    fn_builder.seal_block(after_block);

                                    Ok(ValueOrFunc::Value(result))
                                }
                            } else {
                                Ok(ValueOrFunc::Nothing)
                            }
                        }
                    }
                }

                let target_ty = ast[*target].ty;
                let target = target.compile(ast, checker, compiler, fn_builder)?;

                compile_pattern(&target, target_ty, pattern, ast, checker, compiler, fn_builder)
            }
            &Expr::TypeIndex { ty, path } => match path {
                TypePath::ComplexType(.., Some((vtable_ref, vfunc_ref))) => Ok(ValueOrFunc::FuncRef(compiler.try_import_vfunc(
                    checker.solver.hash_of(ty),
                    vtable_ref,
                    vfunc_ref,
                    fn_builder,
                ))),
                TypePath::ComplexType(.., Some(variant), None) => {
                    if checker.solver.get_info2(ty).is_enum() {
                        let hash = checker.solver.hash_of(ty);
                        let metadata = fn_builder.ins().iconst(compiler.codegen.module.isa().pointer_type(), variant.index() as i64);
                        let enum_value = if compiler.complex_types[&hash].size == 0 {
                            metadata
                        } else {
                            compiler.complex_types[&hash]
                                .instance(variant, &mut compiler.codegen, fn_builder, Vec::new())
                                .unwrap()
                        };

                        return Ok(ValueOrFunc::FatPtr(enum_value, metadata));
                    }

                    Ok(ValueOrFunc::Nothing)
                }
                TypePath::Func(func_ref) => Ok(ValueOrFunc::FuncRef(match compiler.current_funcs.entry(func_ref) {
                    Entry::Occupied(occupied_entry) => *occupied_entry.get(),
                    Entry::Vacant(vacant_entry) => {
                        let func = compiler.codegen.module.declare_func_in_func(compiler.funcs[&func_ref].2, fn_builder.func);

                        *vacant_entry.insert(func)
                    }
                })),
                path => unimplemented!("{path:?}"),
            },
            Expr::Nothing => Ok(ValueOrFunc::Nothing),
        }
    }
}

fn do_println_fat(value: (usize, usize)) {
    println!("{value:?}");
}

fn do_println(value: i64) {
    println!("{value}");
}

fn do_println_f32(value: f32) {
    println!("{value}");
}

fn do_println_bool(value: i8) {
    println!("{}", value == 1);
}

fn do_println_addr(value: *mut ()) {
    println!("{}", value.addr());
}

fn do_println_str(value: &str) {
    println!("{value}");
}
