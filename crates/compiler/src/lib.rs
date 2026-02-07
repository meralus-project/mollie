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
use mollie_typed_ast::{BlockRef, FuncRef, Stmt, StmtRef, TypeChecker, TypedAST, VTableFuncKind};
use mollie_typing::{AdtVariantRef, FieldRef, IntType, PrimitiveType, TraitRef, TypeInfo, TypeInfoRef, TypeSolver, UIntType, VFuncRef, VTableRef};

use crate::{
    allocator::{GcManagedFieldType, TypeLayout},
    error::{CompileError, CompileResult},
    func::{FuncKey, FunctionCompiler},
};

#[derive(Debug)]
pub struct CompiledAdtVariant {
    pub fields: IndexBoxedSlice<FieldRef, (Field, TypeInfoRef)>,
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
            codegen: CodeGenerator::new(
                symbols.into_iter().chain([
                    ("println", do_println as *const u8),
                    ("println_fat", do_println_fat as *const u8),
                    ("println_bool", do_println_bool as *const u8),
                    ("println_addr", do_println_addr as *const u8),
                    ("println_str", do_println_str as *const u8),
                    ("println_float", do_println_f32 as *const u8),
                    ("molalloc", allocator::alloc as *const u8),
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
        FuncCompiler {
            ctx: self.codegen.module.make_context(),
            fn_builder_ctx: FunctionBuilderContext::new(),
            checker: TypeChecker::new(),
            compiler: self,
        }
    }
}

pub struct FuncCompiler<'a, M: Module = JITModule> {
    pub compiler: &'a mut Compiler<M>,
    pub ctx: Context,
    pub fn_builder_ctx: FunctionBuilderContext,
    pub checker: TypeChecker,
}

#[derive(Debug)]
pub enum FuncCompilerError {
    AllocNotFound,
}

impl FuncCompiler<'_> {
    pub fn compile<T: AsRef<str>>(
        &mut self,
        name: T,
        params: Vec<(String, ir::Type, TypeInfoRef)>,
        returns: Vec<(String, ir::Type, TypeInfoRef)>,
        text: T,
    ) -> CompileResult<FuncId> {
        if !returns.is_empty() {
            self.checker.returns.replace(returns[0].2);
        }

        let mut ast = TypedAST::default();
        let block = match self.checker.type_check(text, &mut ast) {
            Ok(block) => block,
            Err(error) => return Err(CompileError::Type(error.0.to_vec())),
        };

        self.checker.solver.finalize();

        for (adt_ref, args) in &ast.used_adt_types {
            if args.iter().any(|arg| self.checker.solver.contains_unknown(*arg)) || args.len() < self.checker.adt_types[*adt_ref].generics {
                continue;
            }

            let kind = self.checker.adt_types[*adt_ref].kind;
            let hash = {
                let mut state = DefaultHasher::new();

                "adt".hash(&mut state);

                adt_ref.hash(&mut state);
                kind.hash(&mut state);

                for &type_arg in args {
                    self.checker.solver.hash_into(&mut state, type_arg);
                }

                state.finish()
            };

            if self.compiler.adt_types.contains_key(&hash) {
                continue;
            }

            let mut size = 0;
            let mut align = 0;

            let variants: IndexBoxedSlice<AdtVariantRef, CompiledAdtVariant> = self.checker.adt_types[*adt_ref]
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
                        align = align.max(self_align);
                    }

                    CompiledAdtVariant {
                        fields: fields.into_boxed_slice(),
                    }
                })
                .collect();

            let gc_managed_fields = variants
                .iter()
                .flat_map(|(variant_ref, variant)| {
                    iter::repeat(variant_ref).zip(variant.fields.values()).filter_map(|(variant_ref, field)| {
                        let info = self.checker.solver.get_info(field.1);

                        if info.is_adt() {
                            Some((variant_ref, field.0.offset.cast_unsigned(), GcManagedFieldType::Regular))
                        } else if let &TypeInfo::Array(element, _) = info {
                            let info = self.checker.solver.get_info(element);

                            if info.is_adt() {
                                Some((variant_ref, field.0.offset.cast_unsigned(), GcManagedFieldType::ArrayOfRegular))
                            } else if info.is_trait() {
                                Some((variant_ref, field.0.offset.cast_unsigned(), GcManagedFieldType::ArrayOfFat))
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    })
                })
                .collect::<Vec<_>>();

            let gc_managed_fields = Vec::leak::<'static>(gc_managed_fields) as &[_];

            self.compiler.adt_types.insert(hash, CompiledAdt {
                variants,
                name: self.checker.adt_types[*adt_ref].name.clone(),
                applied_generics: args.len(),
                type_layout: Box::leak(Box::new(TypeLayout {
                    size: size as usize,
                    align: align as usize,
                    kind: Some(kind),
                    adt_ty: Some(hash),
                    gc_managed_fields,
                })),
            });

            tracing::info!(
                target: "mollie-compiler/adt_types",
                size = size,
                align = align,
                "Compiled `{}{}`",
                self.checker.adt_types[*adt_ref].name.as_deref().unwrap_or_default(),
                if args.is_empty() {
                    String::new()
                } else {
                    format!("<{}>", args.iter().map(|arg| self.checker.short_display_of_type(*arg, None)).join(", "))
                }
            );
        }

        for &func_ref in &ast.used_functions {
            let func = &self.checker.local_functions[func_ref];

            if self.compiler.func_ref_to_func_id.contains_key(&func_ref) {
                continue;
            }

            let mut signature = self.compiler.codegen.module.make_signature();

            let args_fmt = if let TypeInfo::Func(args, returns) = self.checker.solver.get_info(func.ty) {
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

                args.iter().map(|arg| self.checker.display_of_type(arg.inner(), None)).join("")
            } else {
                String::new()
            };

            match &func.kind {
                VTableFuncKind::Local(body) => {
                    let name = format!("{}_{args_fmt}", func.name);

                    let mut compiler = FunctionCompiler::new(name, signature, self.compiler, &self.checker, &mut self.ctx, &mut self.fn_builder_ctx).unwrap();
                    let mut index = 0;

                    for (arg, name) in func.arg_names.iter().enumerate() {
                        let value = compiler.fn_builder.block_params(compiler.entry_block)[index];
                        let ty = compiler.fn_builder.func.signature.params[index].value_type;

                        if let TypeInfo::Func(args, _) = compiler.checker.solver.get_info(func.ty) {
                            let var = compiler.fn_builder.declare_var(ty);

                            compiler.fn_builder.def_var(var, value);

                            if args[arg].as_inner().as_ir_type(&compiler.checker.solver, compiler.compiler.isa()).is_fat() {
                                let value = compiler.fn_builder.block_params(compiler.entry_block)[index + 1];
                                let ty = compiler.fn_builder.func.signature.params[index + 1].value_type;

                                let metadata_var = compiler.fn_builder.declare_var(ty);

                                compiler.fn_builder.def_var(metadata_var, value);
                                compiler
                                    .current_frame_mut()
                                    .insert(name.clone(), func::Variable::new(Var::Fat(var, metadata_var), args[arg].inner()));

                                index += 2;
                            } else {
                                compiler
                                    .current_frame_mut()
                                    .insert(name.clone(), func::Variable::new(Var::Regular(var), args[arg].inner()));

                                if let &TypeInfo::Adt(adt_ref, ..) = compiler.checker.solver.get_info(args[arg].inner())
                                    && compiler.checker.adt_types[adt_ref].collectable
                                {
                                    compiler.fn_builder.ins().call(compiler.context.mark_root, &[value]);
                                }

                                index += 1;
                            }
                        }
                    }

                    let returned = body.compile(&ast, &mut compiler).unwrap();

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
                VTableFuncKind::External(name) => {
                    let func_id = self.compiler.codegen.module.declare_function(name, Linkage::Import, &signature).unwrap();

                    self.compiler.func_ref_to_func_id.insert(func_ref, func_id);
                }
            }

            if let TypeInfo::Func(args, returns) = self.checker.solver.get_info(func.ty) {
                tracing::info!(
                    target: "mollie-compiler/functions",
                    is_postfix = func.postfix,
                    "Compiled {}({}) -> {}",
                    func.name,
                    if args.is_empty() {
                        String::new()
                    } else {
                        args.iter().map(|arg| self.checker.short_display_of_type(arg.inner(), None)).join(", ")
                    },
                    self.checker.short_display_of_type(*returns, None)
                );
            }
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

                    (*g, mem::replace(&mut self.checker.solver.type_infos[*g], val))
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
                            let name = format!("{vtable:?}_{args_fmt}_{}", func.name);

                            let mut compiler =
                                FunctionCompiler::new(name, signature, self.compiler, &self.checker, &mut self.ctx, &mut self.fn_builder_ctx).unwrap();
                            let mut index = 0;

                            for (arg, name) in func.arg_names.iter().enumerate() {
                                let value = compiler.fn_builder.block_params(compiler.entry_block)[index];
                                let ty = compiler.fn_builder.func.signature.params[index].value_type;

                                if let TypeInfo::Func(args, _) = compiler.checker.solver.get_info(func.ty) {
                                    let var = compiler.fn_builder.declare_var(ty);

                                    compiler.fn_builder.def_var(var, value);

                                    if args[arg].as_inner().as_ir_type(&compiler.checker.solver, compiler.compiler.isa()).is_fat() {
                                        let value = compiler.fn_builder.block_params(compiler.entry_block)[index + 1];
                                        let ty = compiler.fn_builder.func.signature.params[index + 1].value_type;

                                        let metadata_var = compiler.fn_builder.declare_var(ty);

                                        compiler.fn_builder.def_var(metadata_var, value);
                                        compiler
                                            .current_frame_mut()
                                            .insert(name.clone(), func::Variable::new(Var::Fat(var, metadata_var), args[arg].inner()));

                                        index += 2;
                                    } else {
                                        compiler
                                            .current_frame_mut()
                                            .insert(name.clone(), func::Variable::new(Var::Regular(var), args[arg].inner()));

                                        if let &TypeInfo::Adt(adt_ref, ..) = compiler.checker.solver.get_info(args[arg].inner())
                                            && compiler.checker.adt_types[adt_ref].collectable
                                        {
                                            compiler.fn_builder.ins().call(compiler.context.mark_root, &[value]);
                                        }

                                        index += 1;
                                    }
                                }
                            }

                            let returned = body.compile(&ast, &mut compiler).unwrap();

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

                            func_id
                        }
                        VTableFuncKind::External(name) => self.compiler.codegen.module.declare_function(name, Linkage::Import, &signature).unwrap(),
                    }
                })
                .collect();

            for (g, g_val) in args {
                self.checker.solver.type_infos[g] = g_val;
            }

            tracing::info!(
                target: "mollie-compiler/virtual_tables",
                "Compiled `{}` for `{}`",
                self.checker.vtables[vtable]
                    .origin_trait
                    .map_or("<impl>", |trait_ref| self.checker.traits[trait_ref].name.as_str()),
                self.checker.short_display_of_type(target_ty, None)
            );

            self.compiler.vtables.insert((hash, vtable), functions);
        }

        let mut signature = self.compiler.codegen.module.make_signature();

        for &(_, ty, _) in &params {
            signature.params.push(ir::AbiParam::new(ty));
        }

        for &(_, ty, _) in &returns {
            signature.returns.push(ir::AbiParam::new(ty));
        }

        let name = name.as_ref();
        let mut compiler = FunctionCompiler::new(name, signature, self.compiler, &self.checker, &mut self.ctx, &mut self.fn_builder_ctx).unwrap();

        for (i, (name, ty, type_info)) in params.into_iter().enumerate() {
            let value = compiler.fn_builder.block_params(compiler.entry_block)[i];

            compiler.var(name, ty, type_info, value);
        }

        for (target_ty, vtable, _) in &ast.used_vtables {
            let hash = compiler.checker.solver.hash_of(*target_ty);
            let trait_ref = compiler.checker.vtables[*vtable].origin_trait;

            if compiler.compiler.trait_to_vtable.contains_key(&(hash, trait_ref)) {
                continue;
            }

            let size = Array {
                element: MollieType::Regular(compiler.compiler.ptr_type()),
            }
            .get_size(compiler.compiler.vtables[&(hash, *vtable)].len() + 1);

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

            for &func in compiler.compiler.vtables[&(hash, *vtable)].values() {
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

pub trait CompileTypedAST<M: Module, T> {
    fn compile(self, ast: &TypedAST, compiler: &mut FunctionCompiler<'_, M>) -> CompileResult<T>;
}

pub trait AsIrType {
    fn as_ir_type(&self, solver: &TypeSolver, isa: &dyn TargetIsa) -> MollieType;
}

impl AsIrType for TypeInfoRef {
    fn as_ir_type(&self, solver: &TypeSolver, isa: &dyn TargetIsa) -> MollieType {
        match solver.get_info(*self) {
            TypeInfo::Unknown(fallback) => fallback.map_or(MollieType::Regular(ir::types::INVALID), |ty| ty.as_ir_type(solver, isa)),
            TypeInfo::Primitive(primitive_type) => match primitive_type {
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
            TypeInfo::Adt(..) | TypeInfo::Func(..) => MollieType::Regular(isa.pointer_type()),
            TypeInfo::Array(..) | TypeInfo::Trait(..) => MollieType::Fat(isa.pointer_type(), isa.pointer_type()),
            TypeInfo::Generic(..) | TypeInfo::Ref(_) => unreachable!(),
        }
    }
}

impl<M: Module> CompileTypedAST<M, MolValue> for StmtRef {
    fn compile(self, ast: &TypedAST, compiler: &mut FunctionCompiler<'_, M>) -> CompileResult<MolValue> {
        match &ast[self] {
            Stmt::Expr(expr_ref) => expr_ref.compile(ast, compiler),
            Stmt::VariableDecl { name, value } => {
                let type_info = ast[*value].ty;
                let ty = type_info.as_ir_type(&compiler.checker.solver, compiler.compiler.isa());
                let value = value.compile(ast, compiler)?;

                match (ty, value) {
                    (MollieType::Regular(ty), MolValue::Value(value)) => {
                        compiler.var(name, ty, type_info, value);

                        if compiler.checker.solver.get_info(type_info).is_adt() {
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
            Stmt::Import(_) => todo!(),
        }
    }
}

impl<M: Module> CompileTypedAST<M, MolValue> for BlockRef {
    fn compile(self, ast: &TypedAST, compiler: &mut FunctionCompiler<'_, M>) -> CompileResult<MolValue> {
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
