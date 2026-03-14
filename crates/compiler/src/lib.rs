#![allow(clippy::result_large_err)]

pub mod allocator;
pub mod error;
pub mod expr;
pub mod func;

use std::{
    hash::{DefaultHasher, Hash, Hasher},
    iter, mem,
};

pub use cranelift;
use cranelift::{
    codegen::{Context, ir, print_errors::pretty_error},
    jit::JITModule,
    module::{DataId, FuncId, Linkage, Module, ModuleError, ModuleResult},
    prelude::{AbiParam, Configurable, FunctionBuilder, FunctionBuilderContext, InstBuilder, Variable, isa::TargetIsa, settings, types},
};
pub use indexmap::IndexMap;
pub use itertools;
use itertools::Itertools;
use mollie_index::{Idx, IndexBoxedSlice, IndexVec};
use mollie_ir::{Array, CodeGenerator, Field, MollieType, Symbol, VTablePtr};
use mollie_lexer::{Lexer, Token};
use mollie_parser::Parser;
use mollie_shared::{Positioned, Span};
use mollie_typed_ast::{Block, BlockRef, Expr, FromParsed, FunctionBody, Stmt, StmtRef, TypedAST, TypedASTContext, UsedItem};
use mollie_typing::{
    AdtVariantRef, CoreTypes, FieldRef, FuncRef, IntType, PrimitiveType, TraitRef, Type, TypeContext, TypeInfo, TypeRef, TypeStorage, UIntType, VFuncRef,
    VTableFunc, VTableGenerator, VTableRef,
};

use crate::{
    allocator::{TypeLayout, TypeLayoutField},
    error::{CompileError, CompileResult},
    func::{FuncKey, FunctionCompiler, FunctionContext},
};

#[derive(Debug)]
pub struct CompiledAdtVariant {
    pub fields: IndexBoxedSlice<FieldRef, (Field, TypeRef)>,
}

#[derive(Debug)]
pub struct CompiledAdt {
    pub type_layout: &'static TypeLayout,
    pub name: Option<String>,
    pub applied_generics: usize,
    pub variants: IndexBoxedSlice<AdtVariantRef, CompiledAdtVariant>,
}

impl CompiledAdt {
    pub fn main_variant(&self) -> &CompiledAdtVariant {
        &self.variants[AdtVariantRef::ZERO]
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Var {
    Regular(Variable),
    Fat(Variable, Variable),
}

type VTable = IndexVec<VFuncRef, FuncId>;

#[derive(Debug)]
pub struct Compiler<M: Module = JITModule> {
    codegen: CodeGenerator<M>,
    core_types: CoreTypes<&'static TypeLayout>,
    adt_types: IndexMap<u64, CompiledAdt>,
    trait_to_vtable: IndexMap<(u64, Option<TraitRef>), DataId>,
    vtables: IndexMap<(u64, VTableRef), VTable>,

    name_to_func_id: IndexMap<String, FuncId>,
    func_id_to_name: IndexMap<FuncId, String>,
    func_ref_to_func_id: IndexMap<FuncRef, FuncId>,
}

impl<M: Module> Compiler<M> {
    fn isa(&self) -> &dyn TargetIsa {
        self.codegen.module.isa()
    }

    pub fn ptr_type(&self) -> ir::Type {
        self.isa().pointer_type()
    }

    pub fn find_adt<T: AsRef<str>>(&self, name: T) -> Option<&CompiledAdt> {
        let name = name.as_ref();

        self.adt_types
            .values()
            .find(|&adt| adt.applied_generics == 0 && adt.name.as_deref() == Some(name))
    }

    pub fn get_adt(&self, hash: u64) -> &CompiledAdt {
        &self.adt_types[&hash]
    }

    pub fn get_adt_variant(&self, hash: u64, variant: AdtVariantRef) -> &CompiledAdtVariant {
        &self.adt_types[&hash].variants[variant]
    }

    pub fn try_get_adt_variant(&self, hash: u64, variant: AdtVariantRef) -> Option<&CompiledAdtVariant> {
        self.adt_types.get(&hash).and_then(|adt| adt.variants.get(variant))
    }

    fn import_fn<T: IntoIterator<Item = ir::Type>>(&mut self, name: &str, params: T) -> ModuleResult<FuncId> {
        self.import_fn2(name, params, [])
    }

    fn import_fn2<T: IntoIterator<Item = ir::Type>, R: IntoIterator<Item = ir::Type>>(&mut self, name: &str, params: T, returns: R) -> ModuleResult<FuncId> {
        let mut signature = self.codegen.module.make_signature();

        signature.params.extend(params.into_iter().map(AbiParam::new));
        signature.returns.extend(returns.into_iter().map(AbiParam::new));

        let id = self.codegen.module.declare_function(name, Linkage::Import, &signature)?;

        self.func_id_to_name.insert(id, name.to_owned());
        self.name_to_func_id.insert(name.to_owned(), id);

        Ok(id)
    }
}

impl Compiler {
    pub fn with_symbols<I: IntoIterator<Item = Symbol>>(symbols: I) -> ModuleResult<Self> {
        let mut flag_builder = settings::builder();

        unsafe {
            flag_builder.set("use_colocated_libcalls", "false").unwrap_unchecked();
            flag_builder.set("opt_level", "speed").unwrap_unchecked();
            flag_builder.set("is_pic", "false").unwrap_unchecked();
            flag_builder.set("preserve_frame_pointers", "true").unwrap_unchecked();
        }

        let mut compiler = Self {
            adt_types: IndexMap::new(),
            vtables: IndexMap::new(),
            core_types: CoreTypes {
                void: TypeLayout::static_of::<bool>(),
                any: TypeLayout::static_of::<bool>(),
                boolean: TypeLayout::static_of::<bool>(),
                int8: TypeLayout::static_of::<i8>(),
                int16: TypeLayout::static_of::<i16>(),
                int32: TypeLayout::static_of::<i32>(),
                int64: TypeLayout::static_of::<i64>(),
                int_size: TypeLayout::static_of::<isize>(),
                uint8: TypeLayout::static_of::<u8>(),
                uint16: TypeLayout::static_of::<u16>(),
                uint32: TypeLayout::static_of::<u32>(),
                uint64: TypeLayout::static_of::<u64>(),
                uint_size: TypeLayout::static_of::<usize>(),
                float: TypeLayout::static_of::<f32>(),
                component: TypeLayout::static_of::<usize>(),
                string: TypeLayout::static_of::<&str>(),
            },
            codegen: CodeGenerator::new(
                symbols.into_iter().chain([
                    ("println", do_println as *const u8),
                    ("println_fat", do_println_fat as *const u8),
                    ("println_bool", do_println_bool as *const u8),
                    ("println_addr", do_println_addr as *const u8),
                    ("println_str", do_println_str as *const u8),
                    ("println_float", do_println_f32 as *const u8),
                    ("molalloc", allocator::alloc as *const u8),
                    ("molalloc_arr", allocator::alloc_array as *const u8),
                    ("molrealloc_arr", allocator::realloc_array as *const u8),
                    ("molmark_root", allocator::mark_root as *const u8),
                    ("molunmark_root", allocator::unmark_root as *const u8),
                ]),
                settings::Flags::new(flag_builder),
            ),
            trait_to_vtable: IndexMap::new(),
            name_to_func_id: IndexMap::new(),
            func_id_to_name: IndexMap::new(),
            func_ref_to_func_id: IndexMap::new(),
        };

        let ptr_type = compiler.codegen.module.isa().pointer_type();

        let prinlnt_id = compiler.import_fn("println", [types::I64])?;

        compiler.import_fn2("molalloc", [ptr_type], [ptr_type])?;
        compiler.import_fn2("molalloc_arr", [ptr_type, ptr_type], [ptr_type])?;
        compiler.import_fn2("molrealloc_arr", [ptr_type, ptr_type], [ptr_type])?;
        compiler.import_fn2("molmark_root", [ptr_type], [])?;
        compiler.import_fn2("molunmark_root", [ptr_type], [])?;

        compiler.import_fn("println_fat", [ptr_type, ptr_type])?;
        compiler.import_fn("println_str", [ptr_type, ptr_type])?;
        compiler.import_fn("println_bool", [types::I8])?;
        compiler.import_fn("println_addr", [types::I64])?;
        compiler.import_fn("println_float", [types::F32])?;

        let println_frame_addr_id = {
            let mut ctx = compiler.codegen.module.make_context();

            let func = compiler
                .codegen
                .module
                .declare_function("println_frame_addr", Linkage::Local, &ctx.func.signature)?;

            let mut fn_builder_ctx = FunctionBuilderContext::new();
            let mut fn_builder = FunctionBuilder::new(&mut ctx.func, &mut fn_builder_ctx);

            let entry_block = fn_builder.create_block();

            fn_builder.append_block_params_for_function_params(entry_block);
            fn_builder.switch_to_block(entry_block);
            fn_builder.seal_block(entry_block);

            let fp = fn_builder.ins().get_frame_pointer(types::I64);
            let println_func = compiler.codegen.module.declare_func_in_func(prinlnt_id, fn_builder.func);

            fn_builder.ins().call(println_func, &[fp]);
            fn_builder.ins().return_(&[]);

            compiler.codegen.module.define_function(func, &mut ctx)?;

            func
        };

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
            ctx.func.signature.returns.push(AbiParam::new(ptr_type));

            let func = compiler.codegen.module.declare_function("get_size", Linkage::Local, &ctx.func.signature)?;

            let mut fn_builder_ctx = FunctionBuilderContext::new();
            let mut fn_builder = FunctionBuilder::new(&mut ctx.func, &mut fn_builder_ctx);

            let entry_block = fn_builder.create_block();

            fn_builder.append_block_params_for_function_params(entry_block);
            fn_builder.switch_to_block(entry_block);
            fn_builder.seal_block(entry_block);

            let ptr = fn_builder.block_params(entry_block)[0];
            let size = fn_builder.ins().load(ptr_type, ir::MemFlags::trusted(), ptr, 0);

            fn_builder.ins().return_(&[size]);

            compiler.codegen.module.define_function(func, &mut ctx)?;

            func
        };

        compiler.func_id_to_name.insert(println_frame_addr_id, "println_frame_addr".to_owned());
        compiler.func_id_to_name.insert(get_type_idx_id, "get_type_idx".to_owned());
        compiler.func_id_to_name.insert(get_size_id, "get_size".to_owned());

        compiler.name_to_func_id.insert("println_frame_addr".to_owned(), println_frame_addr_id);
        compiler.name_to_func_id.insert("get_type_idx".to_owned(), get_type_idx_id);
        compiler.name_to_func_id.insert("get_size".to_owned(), get_size_id);

        Ok(compiler)
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

        self.name_to_func_id.get(name.as_ref()).map(|&func_id| {
            let code = self.codegen.module.get_finalized_function(func_id);

            unsafe { mem::transmute_copy::<mem::ManuallyDrop<*const u8>, T>(&mem::ManuallyDrop::new(code)) }
        })
    }

    pub unsafe fn get_vtable_ptr<T: Copy>(&self, hash: u64, trait_ref: Option<TraitRef>) -> Option<T> {
        let vtable = *self.trait_to_vtable.get(&(hash, trait_ref))?;
        let (vtable_ptr, vtable_size) = self.codegen.module.get_finalized_data(vtable);

        debug_assert_eq!(size_of::<T>() + size_of::<usize>(), vtable_size);

        Some(unsafe { *vtable_ptr.byte_add(size_of::<usize>()).cast() })
    }
}

impl<M: Module> Compiler<M> {
    pub fn start_compiling(&mut self) -> FuncCompiler<'_, M> {
        fn push<M: Module>(
            fn_builder: &mut FunctionBuilder<'_>,
            compiler: &mut Compiler<M>,
            context: FunctionContext,
            entry_block: ir::Block,
            ty: &[MollieType],
        ) -> MolValue {
            let ptr_type = compiler.ptr_type();
            let array_ptr = fn_builder.block_params(entry_block)[0];
            let array_size = fn_builder.ins().load(ptr_type, ir::MemFlags::trusted(), array_ptr, 0);
            let new_array_size = fn_builder.ins().iadd_imm(array_size, 1);

            fn_builder.ins().call(context.realloc_array, &[array_ptr, new_array_size]);

            let array_ptr = fn_builder
                .ins()
                .load(ptr_type, ir::MemFlags::trusted(), array_ptr, size_of::<usize>().cast_signed() as i32 * 2);

            let offset = fn_builder.ins().imul_imm(array_size, i64::from(ty[0].bytes().cast_signed()));
            let ptr = fn_builder.ins().iadd(array_ptr, offset);
            let params = &fn_builder.block_params(entry_block)[1..];
            let value = params[0];
            let metadata = params.get(1).copied();

            fn_builder.ins().store(ir::MemFlags::trusted(), value, ptr, 0);

            if let Some(metadata) = metadata {
                fn_builder.ins().store(ir::MemFlags::trusted(), metadata, ptr, ptr_type.bytes().cast_signed());
            }

            MolValue::Nothing
        }

        let mut context = TypedASTContext::<SpecialCase<M>>::new(TypeContext::new());
        let element = context.type_context.types.get_or_add(Type::Generic(0));
        let this = context.type_context.types.get_or_add(Type::Array(element, None));

        context.vtables.push(IndexVec::from_iter([FunctionBody::External(push::<M> as SpecialCase<M>)]));

        context.type_context.vtables.insert(VTableGenerator {
            ty: this,
            origin_trait: None,
            generics: Box::new([element]),
            functions: IndexVec::from_iter([VTableFunc {
                trait_func: None,
                name: String::from("push"),
                arg_names: vec![String::from("item")],
                ty: context
                    .type_context
                    .types
                    .get_or_add(Type::Func(Box::new([this, element]), context.type_context.core_types.void)),
            }]),
        });

        FuncCompiler {
            ctx: self.codegen.module.make_context(),
            fn_builder_ctx: FunctionBuilderContext::new(),
            context,
            compiler: self,
        }
    }
}

type SpecialCase<M> = fn(&mut FunctionBuilder<'_>, &mut Compiler<M>, FunctionContext, ir::Block, &[MollieType]) -> MolValue;

pub struct FuncCompiler<'a, M: Module = JITModule> {
    pub compiler: &'a mut Compiler<M>,
    pub ctx: Context,
    pub fn_builder_ctx: FunctionBuilderContext,
    pub context: TypedASTContext<SpecialCase<M>>,
}

#[derive(Debug)]
pub enum FuncCompilerError {
    AllocNotFound,
    Module(ModuleError),
}

impl From<ModuleError> for FuncCompilerError {
    fn from(error: ModuleError) -> Self {
        Self::Module(error)
    }
}

impl FuncCompiler<'_> {
    pub fn compile<T: AsRef<str>>(
        &mut self,
        name: T,
        params: Vec<(String, ir::Type, TypeRef)>,
        returns: Option<(ir::Type, TypeRef)>,
        text: T,
    ) -> CompileResult<FuncId> {
        // if !returns.is_empty() {
        //     self.context.returns.replace(returns[0].2);
        // }

        let (ast, block) = {
            let mut ast = TypedAST::default();
            let mut context_ref = self.context.take_ref();

            let (stmts, final_stmt) = mollie_parser::parse_statements_until(&mut Parser::new(Lexer::lex(text)), &Token::EOF).unwrap();
            let mut block_stmts = Vec::new();

            for stmt in stmts {
                if let Some(stmt) = Stmt::from_parsed(stmt.value, &mut ast, &mut context_ref, stmt.span) {
                    block_stmts.push(stmt);
                }
            }

            let (expr, ty) = match final_stmt {
                Some(Positioned {
                    value: mollie_parser::Stmt::Expression(expr),
                    span,
                }) => {
                    let expr = Expr::from_parsed(expr, &mut ast, &mut context_ref, span);

                    (Some(expr), ast[expr].ty)
                }
                _ => (None, context_ref.solver.add_info(TypeInfo::Primitive(PrimitiveType::Void), None)),
            };

            let result = ast.add_block(
                Block {
                    stmts: block_stmts.into_boxed_slice(),
                    expr,
                },
                ty,
                Span::default(),
            );

            let returns = match returns {
                Some(v) => v.1,
                None => context_ref.solver.context.core_types.void,
            };

            ast.solve(result, &mut context_ref, returns)
        };

        if !self.context.type_context.errors.is_empty() {
            return Err(CompileError::Type(mem::take(&mut self.context.type_context.errors).raw));
        }

        for item in &ast.used_items {
            match item {
                UsedItem::VTable(target_ty, vtable, args) => {
                    let target_ty = *target_ty;
                    let vtable = *vtable;
                    let hash = self.context.type_context.types.hash_of(target_ty);

                    if self.compiler.vtables.contains_key(&(hash, vtable)) {
                        continue;
                    }

                    // let args = self.context.vtables[vtable]
                    //     .generics
                    //     .iter()
                    //     .zip(args)
                    //     .map(|(g, a)| {
                    //         let val = self.context.solver.type_infos[*a].clone();

                    //         (*g, mem::replace(&mut self.context.solver.type_infos[*g], val))
                    //     })
                    //     .collect::<Box<[_]>>();

                    let args_fmt = args.iter().map(|arg| self.context.type_context.display_of(*arg)).join("");

                    self.compiler.vtables.insert((hash, vtable), IndexVec::new());
                    self.context.type_context.types.generics.clone_from(args);

                    for (func_ref, func) in self.context.type_context.vtables[vtable].functions.iter() {
                        let mut signature = self.compiler.codegen.module.make_signature();

                        if let Type::Func(args, returns) = &self.context.type_context.types[func.ty] {
                            for arg in args {
                                if arg != &self.context.type_context.core_types.void {
                                    arg.as_ir_type(&self.context.type_context.types, self.compiler.codegen.module.isa())
                                        .add_to_params(&mut signature.params);
                                }
                            }

                            if returns != &self.context.type_context.core_types.void {
                                returns
                                    .as_ir_type(&self.context.type_context.types, self.compiler.codegen.module.isa())
                                    .add_to_params(&mut signature.returns);
                            }
                        }

                        match &self.context.vtables[vtable][func_ref] {
                            FunctionBody::Local { ast, entry } => {
                                let name = format!("{vtable:?}_{args_fmt}_{}", func.name);

                                let mut compiler =
                                    FunctionCompiler::new(name, signature, self.compiler, &self.context, &mut self.ctx, &mut self.fn_builder_ctx).unwrap();
                                let mut index = 0;

                                for (arg, name) in func.arg_names.iter().enumerate() {
                                    let value = compiler.fn_builder.block_params(compiler.entry_block)[index];
                                    let ty = compiler.fn_builder.func.signature.params[index].value_type;

                                    if let Type::Func(args, _) = &compiler.type_context.type_context.types[func.ty] {
                                        let var = compiler.fn_builder.declare_var(ty);

                                        compiler.fn_builder.def_var(var, value);

                                        if args[arg]
                                            .as_ir_type(&compiler.type_context.type_context.types, compiler.compiler.isa())
                                            .is_fat()
                                        {
                                            let value = compiler.fn_builder.block_params(compiler.entry_block)[index + 1];
                                            let ty = compiler.fn_builder.func.signature.params[index + 1].value_type;

                                            let metadata_var = compiler.fn_builder.declare_var(ty);

                                            compiler.fn_builder.def_var(metadata_var, value);
                                            compiler
                                                .current_frame_mut()
                                                .insert(name.clone(), func::Variable::new(Var::Fat(var, metadata_var), args[arg]));

                                            index += 2;
                                        } else {
                                            compiler
                                                .current_frame_mut()
                                                .insert(name.clone(), func::Variable::new(Var::Regular(var), args[arg]));

                                            if let &Type::Adt(adt_ref, ..) = &compiler.type_context.type_context.types[args[arg]]
                                                && compiler.type_context.type_context.adt_types[adt_ref].collectable
                                            {
                                                compiler.fn_builder.ins().call(compiler.context.mark_root, &[value]);
                                            }

                                            index += 1;
                                        }
                                    }
                                }

                                let returned = entry.compile(ast, &mut compiler).unwrap();

                                compiler.unmark_variables();
                                compiler.return_(returned);
                                compiler.fn_builder.finalize();

                                let func_id = compiler.id;

                                if let Err(e) = self.compiler.codegen.module.define_function(func_id, &mut self.ctx) {
                                    match e {
                                        ModuleError::Compilation(error) => {
                                            panic!("function definition failed: {}", pretty_error(&self.ctx.func, error));
                                        }
                                        e => panic!("{}\nfunction definition failed: {e}", self.ctx.func),
                                    }
                                }

                                self.compiler.vtables[&(hash, vtable)].insert(func_id);
                            }
                            FunctionBody::Import(name) => {
                                self.compiler.vtables[&(hash, vtable)]
                                    .insert(self.compiler.codegen.module.declare_function(name, Linkage::Import, &signature).unwrap());
                            }
                            FunctionBody::External(generator) => {
                                let name = format!("{vtable:?}_{args_fmt}_{}", func.name);

                                let mut compiler =
                                    FunctionCompiler::new(name, signature, self.compiler, &self.context, &mut self.ctx, &mut self.fn_builder_ctx).unwrap();
                                let mut index = 0;
                                let mut special_args = Vec::new();

                                for (arg, name) in func.arg_names.iter().enumerate() {
                                    let value = compiler.fn_builder.block_params(compiler.entry_block)[index];
                                    let ty = compiler.fn_builder.func.signature.params[index].value_type;

                                    if let Type::Func(args, _) = &compiler.type_context.type_context.types[func.ty] {
                                        let var = compiler.fn_builder.declare_var(ty);

                                        compiler.fn_builder.def_var(var, value);

                                        let special_type = args[arg].as_ir_type(&compiler.type_context.type_context.types, compiler.compiler.isa());

                                        special_args.push(special_type);

                                        if special_type.is_fat() {
                                            let value = compiler.fn_builder.block_params(compiler.entry_block)[index + 1];
                                            let ty = compiler.fn_builder.func.signature.params[index + 1].value_type;

                                            let metadata_var = compiler.fn_builder.declare_var(ty);

                                            compiler.fn_builder.def_var(metadata_var, value);
                                            compiler
                                                .current_frame_mut()
                                                .insert(name.clone(), func::Variable::new(Var::Fat(var, metadata_var), args[arg]));

                                            index += 2;
                                        } else {
                                            compiler
                                                .current_frame_mut()
                                                .insert(name.clone(), func::Variable::new(Var::Regular(var), args[arg]));

                                            if let &Type::Adt(adt_ref, ..) = &compiler.type_context.type_context.types[args[arg]]
                                                && compiler.type_context.type_context.adt_types[adt_ref].collectable
                                            {
                                                compiler.fn_builder.ins().call(compiler.context.mark_root, &[value]);
                                            }

                                            index += 1;
                                        }
                                    }
                                }

                                let returned = generator(
                                    &mut compiler.fn_builder,
                                    compiler.compiler,
                                    compiler.context,
                                    compiler.entry_block,
                                    &special_args,
                                );

                                compiler.unmark_variables();
                                compiler.return_(returned);
                                compiler.fn_builder.finalize();

                                let func_id = compiler.id;

                                if let Err(e) = self.compiler.codegen.module.define_function(func_id, &mut self.ctx) {
                                    match e {
                                        ModuleError::Compilation(error) => {
                                            panic!("function definition failed: {}", pretty_error(&self.ctx.func, error));
                                        }
                                        e => panic!("{}\nfunction definition failed: {e}", self.ctx.func),
                                    }
                                }

                                self.compiler.vtables[&(hash, vtable)].insert(func_id);
                            }
                        }
                    }

                    self.context.type_context.types.generics = Box::new([]);

                    tracing::info!(
                        target: "mollie-compiler/virtual_tables",
                        "Compiled `{} for `{}`",
                        self.context.type_context.vtables[vtable]
                            .origin_trait
                            .map_or("<impl>", |trait_ref| self.context.type_context.traits[trait_ref].name.as_str()),
                        self.context.type_context.display_of(target_ty),
                    );

                    // for (g, g_val) in args {
                    //     self.context.solver.type_infos[g] = g_val;
                    // }
                }
                UsedItem::Adt(adt_ref, args) => {
                    if args.len() < self.context.type_context.adt_types[*adt_ref].generics {
                        continue;
                    }

                    let kind = self.context.type_context.adt_types[*adt_ref].kind;
                    let hash = {
                        let mut state = DefaultHasher::new();

                        "adt".hash(&mut state);

                        adt_ref.hash(&mut state);

                        for &type_arg in args {
                            self.context.type_context.types.hash_into(&mut state, type_arg);
                        }

                        state.finish()
                    };

                    if self.compiler.adt_types.contains_key(&hash) {
                        continue;
                    }

                    let mut size = 0;
                    let mut align = 0;

                    let variants: IndexBoxedSlice<AdtVariantRef, CompiledAdtVariant> = self.context.type_context.adt_types[*adt_ref]
                        .variants
                        .values()
                        .map(|variant| {
                            let mut fields = <IndexVec<FieldRef, (Field, TypeRef)>>::with_capacity(variant.fields.len());
                            let mut offset = 0;
                            let mut variant_size = 0;
                            let mut self_align = 0;

                            for field in variant.fields.values() {
                                let type_info = self.context.type_context.types.apply_type_args(field.ty, args);
                                let ty = type_info.as_ir_type(&self.context.type_context.types, self.compiler.codegen.module.isa());
                                let ty_size = ty.bytes();

                                variant_size += ty_size;

                                let align = ty_size;
                                let padding = (align - variant_size % align) % align;

                                variant_size += padding;

                                fields.push((
                                    Field {
                                        ty,
                                        offset,
                                        default_value: field.default_value.clone(),
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
                                align = align.max(self_align);
                            }

                            CompiledAdtVariant {
                                fields: fields.into_boxed_slice(),
                            }
                        })
                        .collect();

                    let fields = variants
                        .iter()
                        .flat_map(|(variant_ref, variant)| {
                            iter::repeat(variant_ref).zip(variant.fields.values()).map(|(variant_ref, field)| {
                                let ty = &self.context.type_context.types[field.1];
                                let offset = field.0.offset.cast_unsigned();
                                let ir_type = field.1.as_ir_type(&self.context.type_context.types, self.compiler.isa());

                                if matches!(ty, Type::Adt(..)) {
                                    (variant_ref, offset, ir_type, TypeLayoutField::Collectable)
                                } else if let &Type::Array(element, _) = ty {
                                    let info = &self.context.type_context.types[element];

                                    if matches!(info, Type::Adt(..)) {
                                        (variant_ref, offset, ir_type, TypeLayoutField::ArrayOfRegular)
                                    } else if matches!(info, Type::Trait(..)) {
                                        (variant_ref, offset, ir_type, TypeLayoutField::ArrayOfFat)
                                    } else {
                                        (variant_ref, offset, ir_type, TypeLayoutField::Regular)
                                    }
                                } else {
                                    (variant_ref, offset, ir_type, TypeLayoutField::Regular)
                                }
                            })
                        })
                        .collect::<Vec<_>>();

                    let fields = Vec::leak::<'static>(fields) as &[_];

                    self.compiler.adt_types.insert(hash, CompiledAdt {
                        variants,
                        name: self.context.type_context.adt_types[*adt_ref].name.clone(),
                        applied_generics: args.len(),
                        type_layout: Box::leak(Box::new(TypeLayout {
                            size: size as usize,
                            align: align as usize,
                            kind: Some(kind),
                            adt_ty: Some(hash),
                            fields,
                        })),
                    });

                    tracing::info!(
                        target: "mollie-compiler/adt_types     ",
                        size = size,
                        align = align,
                        "Compiled `{}{}`",
                        self.context.type_context.adt_types[*adt_ref].name.as_deref().unwrap_or_default(),
                        if args.is_empty() {
                            String::new()
                        } else {
                            format!("<{}>", args.iter().map(|arg| self.context.type_context.display_of(*arg)).join(", "))
                        }
                    );
                }
                &UsedItem::Func(func_ref) => {
                    let func = &self.context.type_context.functions[func_ref];

                    if self.compiler.func_ref_to_func_id.contains_key(&func_ref) {
                        continue;
                    }

                    let mut signature = self.compiler.codegen.module.make_signature();

                    let args_fmt = if let Type::Func(args, returns) = &self.context.type_context.types[func.ty] {
                        for arg in args {
                            if arg != &self.context.type_context.core_types.void {
                                arg.as_ir_type(&self.context.type_context.types, self.compiler.codegen.module.isa())
                                    .add_to_params(&mut signature.params);
                            }
                        }

                        if returns != &self.context.type_context.core_types.void {
                            returns
                                .as_ir_type(&self.context.type_context.types, self.compiler.codegen.module.isa())
                                .add_to_params(&mut signature.returns);
                        }

                        args.iter().map(|arg| self.context.type_context.display_of(*arg)).join("")
                    } else {
                        String::new()
                    };

                    match &self.context.functions[func_ref] {
                        FunctionBody::Local { ast, entry } => {
                            let name = format!("{}_{args_fmt}", func.name);

                            let mut compiler =
                                FunctionCompiler::new(name, signature, self.compiler, &self.context, &mut self.ctx, &mut self.fn_builder_ctx).unwrap();
                            let mut index = 0;

                            for (arg, name) in func.arg_names.iter().enumerate() {
                                let value = compiler.fn_builder.block_params(compiler.entry_block)[index];
                                let ty = compiler.fn_builder.func.signature.params[index].value_type;

                                if let Type::Func(args, _) = &compiler.type_context.type_context.types[func.ty] {
                                    let var = compiler.fn_builder.declare_var(ty);

                                    compiler.fn_builder.def_var(var, value);

                                    if args[arg]
                                        .as_ir_type(&compiler.type_context.type_context.types, compiler.compiler.isa())
                                        .is_fat()
                                    {
                                        let value = compiler.fn_builder.block_params(compiler.entry_block)[index + 1];
                                        let ty = compiler.fn_builder.func.signature.params[index + 1].value_type;

                                        let metadata_var = compiler.fn_builder.declare_var(ty);

                                        compiler.fn_builder.def_var(metadata_var, value);
                                        compiler
                                            .current_frame_mut()
                                            .insert(name.clone(), func::Variable::new(Var::Fat(var, metadata_var), args[arg]));

                                        index += 2;
                                    } else {
                                        compiler
                                            .current_frame_mut()
                                            .insert(name.clone(), func::Variable::new(Var::Regular(var), args[arg]));

                                        if let &Type::Adt(adt_ref, ..) = &compiler.type_context.type_context.types[args[arg]]
                                            && compiler.type_context.type_context.adt_types[adt_ref].collectable
                                        {
                                            compiler.fn_builder.ins().call(compiler.context.mark_root, &[value]);
                                        }

                                        index += 1;
                                    }
                                }
                            }

                            let returned = entry.compile(ast, &mut compiler).unwrap();

                            compiler.unmark_variables();
                            compiler.return_(returned);
                            compiler.fn_builder.finalize();

                            let func_id = compiler.id;

                            if let Err(e) = self.compiler.codegen.module.define_function(func_id, &mut self.ctx) {
                                match e {
                                    ModuleError::Compilation(error) => {
                                        panic!("function definition failed: {}", pretty_error(&self.ctx.func, error));
                                    }
                                    e => panic!("{}\nfunction definition failed: {e}", self.ctx.func),
                                }
                            }

                            self.compiler.func_ref_to_func_id.insert(func_ref, func_id);
                        }
                        FunctionBody::Import(name) => {
                            let func_id = self.compiler.codegen.module.declare_function(name, Linkage::Import, &signature).unwrap();

                            self.compiler.func_ref_to_func_id.insert(func_ref, func_id);
                        }
                        FunctionBody::External(_) => todo!(),
                    }

                    if let Type::Func(args, returns) = &self.context.type_context.types[func.ty] {
                        tracing::info!(
                            target: "mollie-compiler/functions     ",
                            is_postfix = func.postfix,
                            "Compiled `{}({}) -> {}`",
                            func.name,
                            if args.is_empty() {
                                String::new()
                            } else {
                                args.iter().map(|arg| self.context.type_context.display_of(*arg)).join(", ")
                            },
                            self.context.type_context.display_of(*returns)
                        );
                    }
                }
            }
        }

        let mut signature = self.compiler.codegen.module.make_signature();

        for &(_, ty, _) in &params {
            signature.params.push(ir::AbiParam::new(ty));
        }

        if let Some((ty, _)) = returns {
            signature.returns.push(ir::AbiParam::new(ty));
        }

        let name = name.as_ref();
        let mut compiler = FunctionCompiler::new(name, signature, self.compiler, &self.context, &mut self.ctx, &mut self.fn_builder_ctx).unwrap();

        for (i, (name, ty, type_info)) in params.into_iter().enumerate() {
            let value = compiler.fn_builder.block_params(compiler.entry_block)[i];

            compiler.var(name, ty, type_info, value);
        }

        for used_item in &ast.used_items {
            if let &UsedItem::VTable(target_ty, vtable, _) = used_item {
                let hash = compiler.type_context.type_context.types.hash_of(target_ty);
                let trait_ref = compiler.type_context.type_context.vtables[vtable].origin_trait;

                if compiler.compiler.trait_to_vtable.contains_key(&(hash, trait_ref)) {
                    continue;
                }

                let size = Array {
                    element: MollieType::Regular(compiler.compiler.ptr_type()),
                }
                .get_size(compiler.compiler.vtables[&(hash, vtable)].len() + 1);

                compiler.compiler.codegen.data_desc.define_zeroinit(size as usize);

                let id = compiler.compiler.codegen.module.declare_anonymous_data(true, false).unwrap();

                compiler.compiler.codegen.module.define_data(id, &compiler.compiler.codegen.data_desc).unwrap();
                compiler.compiler.codegen.data_desc.clear();

                let ptr_type = compiler.compiler.ptr_type();

                let data_id = compiler.compiler.codegen.module.declare_data_in_func(id, compiler.fn_builder.func);
                let vtable_ptr = compiler.fn_builder.ins().global_value(ptr_type, data_id);
                let type_id = compiler.fn_builder.ins().iconst(ptr_type, hash.cast_signed());

                compiler.fn_builder.ins().store(ir::MemFlags::trusted(), type_id, vtable_ptr, 0);

                let mut offset = ptr_type.bytes();

                for &func in compiler.compiler.vtables[&(hash, vtable)].values() {
                    let func_ref = compiler.compiler.codegen.module.declare_func_in_func(func, compiler.fn_builder.func);

                    compiler.funcs.insert(FuncKey::Id(func), func_ref);

                    let func_ptr = compiler.fn_builder.ins().func_addr(ptr_type, func_ref);

                    compiler
                        .fn_builder
                        .ins()
                        .store(ir::MemFlags::trusted(), func_ptr, vtable_ptr, offset.cast_signed());

                    offset += ptr_type.bytes();
                }

                compiler.compiler.trait_to_vtable.insert((hash, trait_ref), id);
            }
        }

        let returned = block.compile(&ast, &mut compiler).unwrap();

        compiler.return_(returned);
        compiler.fn_builder.finalize();

        let func = compiler.id;

        if let Err(e) = self.compiler.codegen.module.define_function(func, &mut self.ctx) {
            match e {
                ModuleError::Compilation(error) => {
                    panic!("function definition failed: {}", pretty_error(&self.ctx.func, error));
                }
                e => panic!("{}\nfunction definition failed: {e}", self.ctx.func),
            }
        }

        self.compiler.codegen.module.finalize_definitions().unwrap();
        self.compiler.name_to_func_id.insert(name.into(), func);

        Ok(func)
    }
}

#[derive(Debug, Clone)]
pub enum MolValue {
    Value(ir::Value),
    Values(Vec<ir::Value>),
    FuncRef(ir::FuncRef),
    CaptureFuncRef(ir::FuncRef, ir::Value),
    FatPtr(ir::Value, ir::Value),
    Nothing,
}

impl MolValue {
    pub fn expect_value(self) -> ir::Value {
        match self {
            Self::Value(value) => value,
            value => panic!("MolValue::Value was expected, found {value:?}"),
        }
    }
}

pub trait CompileTypedAST<S, M: Module, T> {
    fn compile(self, ast: &TypedAST, compiler: &mut FunctionCompiler<'_, S, M>) -> CompileResult<T>;
}

pub trait AsIrType {
    fn as_ir_type(&self, context: &TypeStorage, isa: &dyn TargetIsa) -> MollieType;
}

impl AsIrType for TypeRef {
    fn as_ir_type(&self, context: &TypeStorage, isa: &dyn TargetIsa) -> MollieType {
        match &context[*self] {
            Type::Primitive(primitive_type) => match primitive_type {
                PrimitiveType::Any => unimplemented!(),
                PrimitiveType::Int(IntType::ISize) | PrimitiveType::UInt(UIntType::USize) => MollieType::Regular(isa.pointer_type()),
                PrimitiveType::String | PrimitiveType::Component => MollieType::Fat(isa.pointer_type(), isa.pointer_type()),
                PrimitiveType::Int(IntType::I64) | PrimitiveType::UInt(UIntType::U64) => MollieType::Regular(ir::types::I64),
                PrimitiveType::Int(IntType::I32) | PrimitiveType::UInt(UIntType::U32) => MollieType::Regular(ir::types::I32),
                PrimitiveType::Int(IntType::I16) | PrimitiveType::UInt(UIntType::U16) => MollieType::Regular(ir::types::I16),
                PrimitiveType::Int(IntType::I8) | PrimitiveType::UInt(UIntType::U8) | PrimitiveType::Boolean => MollieType::Regular(ir::types::I8),
                PrimitiveType::Float => MollieType::Regular(ir::types::F32),
                PrimitiveType::Void => unimplemented!(),
                PrimitiveType::Null => unimplemented!(),
            },
            Type::Array(..) | Type::Adt(..) | Type::Func(..) => MollieType::Regular(isa.pointer_type()),
            Type::Trait(..) => MollieType::Fat(isa.pointer_type(), isa.pointer_type()),
            &Type::Generic(i) => context.generics.get(i).map_or_else(|| unimplemented!(), |ty| ty.as_ir_type(context, isa)),
            Type::Error => unreachable!(),
        }
    }
}

impl<S, M: Module> CompileTypedAST<S, M, MolValue> for StmtRef {
    fn compile(self, ast: &TypedAST, compiler: &mut FunctionCompiler<'_, S, M>) -> CompileResult<MolValue> {
        match &ast[self] {
            Stmt::Expr(expr_ref) => expr_ref.compile(ast, compiler),
            Stmt::NewVar { name, value, .. } => {
                let type_info = ast[*value].ty;
                let ty = type_info.as_ir_type(&compiler.type_context.type_context.types, compiler.compiler.isa());
                let value = value.compile(ast, compiler)?;

                match (ty, value) {
                    (MollieType::Regular(ty), MolValue::Value(value)) => {
                        compiler.var(name, ty, type_info, value);

                        if matches!(compiler.type_context.type_context.types[type_info], Type::Adt(..)) {
                            compiler.fn_builder.ins().call(compiler.context.mark_root, &[value]);
                        }
                    }
                    (MollieType::Regular(ty), MolValue::FuncRef(value)) => {
                        let value = compiler.fn_builder.ins().func_addr(ty, value);

                        compiler.var(name, ty, type_info, value);
                    }
                    (MollieType::Regular(ty), MolValue::CaptureFuncRef(value, metadata)) => {
                        let value = compiler.fn_builder.ins().func_addr(ty, value);

                        compiler.fat_var(name, ty, ty, type_info, value, metadata);
                    }
                    (MollieType::Fat(ty, fat_ty), MolValue::FatPtr(value, metadata)) => compiler.fat_var(name, ty, fat_ty, type_info, value, metadata),
                    (ty, val) => panic!("can't create variable called {name} with type {ty:?} and value = {val:?}"),
                }

                Ok(MolValue::Nothing)
            }
        }
    }
}

impl<S, M: Module> CompileTypedAST<S, M, MolValue> for BlockRef {
    fn compile(self, ast: &TypedAST, compiler: &mut FunctionCompiler<'_, S, M>) -> CompileResult<MolValue> {
        compiler.push_frame();

        for statement in &ast[self].value.stmts {
            statement.compile(ast, compiler)?;
        }

        let returned = ast[self].value.expr.map_or(Ok(MolValue::Nothing), |expr| expr.compile(ast, compiler))?;

        compiler.unmark_variables();
        compiler.pop_frame();

        Ok(returned)
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
