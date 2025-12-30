#![allow(clippy::result_large_err)]

use std::{
    collections::HashMap,
    fmt,
    hash::{DefaultHasher, Hash, Hasher},
    iter::repeat,
    mem,
};

pub use cranelift;
use cranelift::{
    codegen::{Context, ir},
    jit::{JITBuilder, JITModule},
    module::{DataDescription, FuncId, Linkage, Module, ModuleResult, default_libcall_names},
    native,
    prelude::{
        AbiParam, Configurable, FloatCC, FunctionBuilder, FunctionBuilderContext, InstBuilder, IntCC, Signature, Variable, isa::TargetIsa, settings, types,
    },
};
pub use indexmap::IndexMap;
use mollie_index::{Idx, IndexVec};
use mollie_ir::{Array, FatPtr, Field, VTablePtr, compile_constant};
use mollie_lexer::{Lexer, Token};
use mollie_parser::{BlockExpr, Parser, parse_statements_until};
use mollie_shared::{Operator, Span};
use mollie_tast::{BlockRef, Expr, ExprRef, IntoTypedAST, IsPattern, LiteralExpr, Stmt, StmtRef, TypeChecker, TypedAST, VFunc, VTableFuncKind};
use mollie_typing::{
    CompiledComplexType, CompiledComplexTypeVariant, ComplexType, ComplexTypeRef, ComplexTypeVariantRef, CoreTypes, FieldRef, PrimitiveType, TypeInfo,
    TypeInfoRef, TypeSolver, VFuncRef, VTableRef,
};

pub use self::error::{CompileError, CompileResult};

mod error;

type Symbol = (&'static str, *const u8);

pub struct CodeGenerator<M: Module> {
    pub module: M,
    pub data_desc: DataDescription,
}

impl<M: Module> fmt::Debug for CodeGenerator<M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("JitCompiler").field("data_desc", &self.data_desc).finish_non_exhaustive()
    }
}

impl CodeGenerator<JITModule> {
    fn new<I: IntoIterator<Item = Symbol>>(symbols: I, flags: settings::Flags) -> Self {
        let mut builder = JITBuilder::with_isa(native::builder().unwrap().finish(flags).unwrap(), default_libcall_names());

        builder.symbol("println", do_println as *const u8);
        builder.symbol("println_bool", do_println_bool as *const u8);
        builder.symbol("println_addr", do_println_addr as *const u8);
        builder.symbol("println_str", do_println_str as *const u8);
        builder.symbol("println_float", do_println_f32 as *const u8);

        for (name, ptr) in symbols {
            builder.symbol(name, ptr);
        }

        Self {
            module: JITModule::new(builder),
            data_desc: DataDescription::new(),
        }
    }
}

#[derive(Debug)]
pub struct Compiler<T: Module = JITModule> {
    complex_types: IndexMap<u64, CompiledComplexType>,
    vtables: IndexVec<VTableRef, IndexVec<VFuncRef, (ir::SigRef, ir::FuncRef, FuncId)>>,

    assign_ref: Option<ExprRef>,
    this: Option<ValueOrFunc>,
    values: IndexMap<String, ValueOrFunc>,
    pub variables: IndexMap<String, Variable>,
    globals: IndexMap<String, FuncId>,
    func_names: IndexMap<FuncId, String>,

    codegen: CodeGenerator<T>,
}

impl Default for Compiler {
    fn default() -> Self {
        Self::with_symbols(Vec::new())
    }
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

pub struct FuncCompilerBuilder<'a> {
    pub compiler: &'a mut Compiler,
    pub ctx: Context,
    pub fn_builder_ctx: FunctionBuilderContext,
    pub checker: TypeChecker,
}

impl FuncCompilerBuilder<'_> {
    /// Creates a compiler for the main function, returning [`FuncCompiler`].
    ///
    /// # Errors
    ///
    /// Returns [`ModuleError`] in case of an error during function declaration
    /// or definition.
    ///
    /// [`ModuleError`]: cranelift::module::ModuleError
    pub fn provide(&mut self) -> ModuleResult<FuncCompiler<'_, '_>> {
        self.create_func_compiler(self.compiler.codegen.module.make_signature())
    }

    /// Creates a compiler for the main function with the signature specified in
    /// `signature`, returning [`FuncCompiler`].
    ///
    /// # Errors
    ///
    /// Returns [`ModuleError`] in case of an error during function declaration
    /// or definition.
    ///
    /// [`ModuleError`]: cranelift::module::ModuleError
    pub fn provide_with_signature<F: FnOnce(&mut Signature)>(&mut self, func: F) -> ModuleResult<FuncCompiler<'_, '_>> {
        let mut signature = self.compiler.codegen.module.make_signature();

        func(&mut signature);

        self.create_func_compiler(signature)
    }

    fn create_func_compiler(&mut self, signature: Signature) -> ModuleResult<FuncCompiler<'_, '_>> {
        let ctx = &mut self.ctx;
        let fn_builder_ctx = &mut self.fn_builder_ctx;

        self.compiler.import_fn("println", [types::I64])?;
        self.compiler.import_fn("println_str", [types::I64])?;
        self.compiler.import_fn("println_bool", [types::I8])?;
        self.compiler.import_fn("println_addr", [types::I64])?;
        self.compiler.import_fn("println_float", [types::F32])?;

        let get_type_idx_id = {
            let mut get_type_idx_sig = self.compiler.codegen.module.make_signature();

            get_type_idx_sig.params.push(AbiParam::new(self.compiler.codegen.module.isa().pointer_type()));
            get_type_idx_sig.returns.push(AbiParam::new(self.compiler.codegen.module.isa().pointer_type()));

            let func = self
                .compiler
                .codegen
                .module
                .declare_function("get_type_idx", Linkage::Local, &get_type_idx_sig)?;

            ctx.func.signature = get_type_idx_sig;

            let mut fn_builder_ctx = FunctionBuilderContext::new();
            let mut fn_builder = FunctionBuilder::new(&mut ctx.func, &mut fn_builder_ctx);

            let entry_block = fn_builder.create_block();

            fn_builder.append_block_params_for_function_params(entry_block);
            fn_builder.switch_to_block(entry_block);
            fn_builder.seal_block(entry_block);

            let fat_ptr = fn_builder.block_params(entry_block)[0];
            let vtable_ptr = FatPtr::get_metadata(self.compiler.codegen.module.isa(), &mut fn_builder, fat_ptr);
            let type_idx = VTablePtr::get_type_idx(self.compiler.codegen.module.isa(), &mut fn_builder, vtable_ptr);

            fn_builder.ins().return_(&[type_idx]);

            self.compiler.codegen.module.define_function(func, ctx)?;
            self.compiler.codegen.module.clear_context(ctx);

            func
        };

        let get_size_id = {
            let mut get_size_sig = self.compiler.codegen.module.make_signature();

            get_size_sig.params.push(AbiParam::new(self.compiler.codegen.module.isa().pointer_type()));
            get_size_sig.returns.push(AbiParam::new(self.compiler.codegen.module.isa().pointer_type()));

            let func = self.compiler.codegen.module.declare_function("get_size", Linkage::Local, &get_size_sig)?;

            ctx.func.signature = get_size_sig;

            let mut fn_builder_ctx = FunctionBuilderContext::new();
            let mut fn_builder = FunctionBuilder::new(&mut ctx.func, &mut fn_builder_ctx);

            let entry_block = fn_builder.create_block();

            fn_builder.append_block_params_for_function_params(entry_block);
            fn_builder.switch_to_block(entry_block);
            fn_builder.seal_block(entry_block);

            let fat_ptr = fn_builder.block_params(entry_block)[0];
            let size = FatPtr::get_metadata(self.compiler.codegen.module.isa(), &mut fn_builder, fat_ptr);

            fn_builder.ins().return_(&[size]);

            self.compiler.codegen.module.define_function(func, ctx)?;
            self.compiler.codegen.module.clear_context(ctx);

            func
        };

        self.compiler.func_names.insert(get_type_idx_id, "get_type_idx".to_owned());
        self.compiler.func_names.insert(get_size_id, "get_size".to_owned());

        self.compiler.globals.insert("get_type_idx".to_owned(), get_type_idx_id);
        self.compiler.globals.insert("get_size".to_owned(), get_size_id);

        let mut fn_builder = FunctionBuilder::new(&mut ctx.func, fn_builder_ctx);

        fn_builder.func.signature = signature;

        let entry_block = fn_builder.create_block();

        fn_builder.append_block_params_for_function_params(entry_block);
        fn_builder.switch_to_block(entry_block);
        fn_builder.seal_block(entry_block);

        Ok(FuncCompiler {
            compiler: self.compiler,
            fn_builder,
            checker: &mut self.checker,
        })
    }
}

pub struct FuncCompiler<'a, 'b> {
    pub compiler: &'a mut Compiler,
    pub fn_builder: FunctionBuilder<'b>,
    pub checker: &'a mut TypeChecker,
}

impl FuncCompiler<'_, '_> {
    /// # Errors
    ///
    /// Returns `CompileError` if program parsing or compilation fails.
    pub fn compile_program_text<T: AsRef<str>>(&mut self, text: T) -> CompileResult<FuncId> {
        let mut parser = Parser::new(Lexer::lex(text.as_ref()));

        let program = match parse_statements_until(&mut parser, &Token::EOF) {
            Ok(statements) => statements,
            Err(error) => return Err(CompileError::Parse(error)),
        };

        let mut ast = TypedAST {
            blocks: IndexVec::new(),
            statements: IndexVec::new(),
            exprs: IndexVec::new(),
        };

        let block = BlockExpr {
            stmts: program.0,
            final_stmt: program.1.map(Box::new),
        }
        .into_typed_ast(self.checker, &mut ast, Span::default())
        .unwrap();

        self.checker.solver.finalize();

        let complex_types = (0..self.checker.solver.len())
            .map(TypeInfoRef::new)
            .filter_map(|info| {
                if let TypeInfo::Complex(complex_type, _, args) = self.checker.solver.get_info(info) {
                    if args.iter().any(|arg| self.checker.solver.contains_unknown(*arg)) || args.len() < self.checker.complex_types[*complex_type].generics {
                        None
                    } else {
                        let hash = self.checker.solver.hash_of(info);
                        let args = args.clone();
                        let mut size = 0;

                        Some((hash, CompiledComplexType {
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
                        }))
                    }
                } else {
                    None
                }
            })
            .collect();

        self.compiler.complex_types = complex_types;

        self.compiler.vtables = self
            .checker
            .vtables
            .values()
            .map(|vtables| {
                vtables
                    .values()
                    .map(|func| {
                        let values = mem::take(&mut self.compiler.values);
                        let mut signature = self.compiler.codegen.module.make_signature();

                        if let TypeInfo::Func(args, returns) = self.checker.solver.get_info(func.ty) {
                            for arg in args {
                                if arg != &self.checker.core_types.void {
                                    signature
                                        .params
                                        .push(AbiParam::new(arg.as_ir_type(&self.checker.solver, self.compiler.codegen.module.isa())));
                                }
                            }

                            if returns != &self.checker.core_types.void {
                                signature
                                    .returns
                                    .push(AbiParam::new(returns.as_ir_type(&self.checker.solver, self.compiler.codegen.module.isa())));
                            }
                        }

                        match &func.kind {
                            VTableFuncKind::Local(body) => {
                                let mut ctx = self.compiler.codegen.module.make_context();
                                let func_id = self
                                    .compiler
                                    .codegen
                                    .module
                                    .declare_function(
                                        &func.name,
                                        Linkage::Local, // if self.value.func_vis.is_some() { Linkage::Export } else { Linkage::Local },
                                        &signature,
                                    )
                                    .unwrap();

                                self.compiler.func_names.insert(func_id, func.name.clone());

                                ctx.func.signature = signature.clone();

                                let mut fn_builder_ctx = FunctionBuilderContext::new();
                                let mut fn_builder = FunctionBuilder::new(&mut ctx.func, &mut fn_builder_ctx);

                                let entry_block = fn_builder.create_block();

                                fn_builder.append_block_params_for_function_params(entry_block);
                                fn_builder.switch_to_block(entry_block);
                                fn_builder.seal_block(entry_block);

                                for (index, name) in func.arg_names.iter().enumerate() {
                                    let value = fn_builder.block_params(entry_block)[index];
                                    let ty = fn_builder.func.signature.params[index].value_type;

                                    let var = fn_builder.declare_var(ty);

                                    fn_builder.def_var(var, value);

                                    self.compiler.variables.insert(name.clone(), var);
                                }

                                let result = body.compile(&ast, &self.checker, self.compiler, &mut fn_builder).unwrap();

                                match result {
                                    ValueOrFunc::Value(value) => fn_builder.ins().return_(&[value]),
                                    ValueOrFunc::Values(values) => fn_builder.ins().return_(&values),
                                    _ => fn_builder.ins().return_(&[]),
                                };

                                self.compiler.codegen.module.define_function(func_id, &mut ctx).unwrap();
                                self.compiler.codegen.module.clear_context(&mut ctx);
                                self.compiler.codegen.module.finalize_definitions().unwrap();

                                for name in &func.arg_names {
                                    self.compiler.variables.shift_remove(name);
                                }

                                let signature = self.fn_builder.import_signature(signature);
                                let func = self.compiler.codegen.module.declare_func_in_func(func_id, self.fn_builder.func);

                                self.compiler.values = values;

                                (signature, func, func_id)
                            }
                            VTableFuncKind::External(name) => {
                                let func_id = self.compiler.codegen.module.declare_function(name, Linkage::Import, &signature).unwrap();

                                let signature = self.fn_builder.import_signature(signature);
                                let func = self.compiler.codegen.module.declare_func_in_func(func_id, self.fn_builder.func);

                                (signature, func, func_id)
                            }
                        }
                    })
                    .collect()
            })
            .collect();

        match block.compile(&ast, &self.checker, self.compiler, &mut self.fn_builder).unwrap() {
            ValueOrFunc::Value(value) => {
                self.fn_builder.ins().return_(&[value]);
            }
            ValueOrFunc::Values(values) => {
                self.fn_builder.ins().return_(&values);
            }
            _ => {
                self.fn_builder.ins().return_(&[]);
            }
        }

        Ok(self
            .compiler
            .codegen
            .module
            .declare_function("<main>", Linkage::Export, &self.fn_builder.func.signature)
            .unwrap())
    }

    pub fn register_complex_type(&mut self, ty: ComplexType) -> ComplexTypeRef {
        let type_ref = self.checker.complex_types.insert(ty);

        if let Some(name) = self.checker.complex_types[type_ref].name.clone() {
            self.checker.name_to_complex_type.insert(name, type_ref);
        }

        type_ref
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

    pub fn signature_mut(&mut self) -> &mut Signature {
        &mut self.fn_builder.func.signature
    }

    pub fn set_main_signature(&mut self, signature: Signature) {
        self.fn_builder.func.signature = signature;
    }
}

impl Compiler {
    pub fn with_symbols<I: IntoIterator<Item = Symbol>>(symbols: I) -> Self {
        let mut flag_builder = settings::builder();

        unsafe {
            flag_builder.set("use_colocated_libcalls", "false").unwrap_unchecked();
            flag_builder.set("opt_level", "speed").unwrap_unchecked();
            flag_builder.set("is_pic", "false").unwrap_unchecked();
        }

        Self {
            complex_types: IndexMap::new(),
            vtables: IndexVec::new(),
            assign_ref: None,
            this: None,
            codegen: CodeGenerator::new(symbols, settings::Flags::new(flag_builder)),
            values: IndexMap::new(),
            globals: IndexMap::new(),
            variables: IndexMap::new(),
            func_names: IndexMap::new(),
        }
    }

    fn get<T: AsRef<str>>(&self, name: T) -> Option<ValueOrFunc> {
        self.values.get(name.as_ref()).cloned()
    }

    pub fn start_compiling(&mut self) -> FuncCompilerBuilder<'_> {
        let ctx = self.codegen.module.make_context();
        let fn_builder_ctx = FunctionBuilderContext::new();

        let mut solver = TypeSolver::default();

        solver.push_frame();

        let core_types = CoreTypes {
            void: solver.add_info(TypeInfo::Primitive(PrimitiveType::Void)),
            any: solver.add_info(TypeInfo::Primitive(PrimitiveType::Any)),
            boolean: solver.add_info(TypeInfo::Primitive(PrimitiveType::Boolean)),
            int8: solver.add_info(TypeInfo::Primitive(PrimitiveType::I8)),
            int16: solver.add_info(TypeInfo::Primitive(PrimitiveType::I16)),
            int32: solver.add_info(TypeInfo::Primitive(PrimitiveType::I32)),
            int64: solver.add_info(TypeInfo::Primitive(PrimitiveType::I64)),
            int_size: solver.add_info(TypeInfo::Primitive(PrimitiveType::ISize)),
            uint8: solver.add_info(TypeInfo::Primitive(PrimitiveType::U8)),
            uint16: solver.add_info(TypeInfo::Primitive(PrimitiveType::U16)),
            uint32: solver.add_info(TypeInfo::Primitive(PrimitiveType::U32)),
            uint64: solver.add_info(TypeInfo::Primitive(PrimitiveType::U64)),
            uint_size: solver.add_info(TypeInfo::Primitive(PrimitiveType::USize)),
            float: solver.add_info(TypeInfo::Primitive(PrimitiveType::Float)),
            component: solver.add_info(TypeInfo::Primitive(PrimitiveType::Component)),
            string: solver.add_info(TypeInfo::Primitive(PrimitiveType::String)),
        };

        FuncCompilerBuilder {
            compiler: self,
            ctx,
            fn_builder_ctx,
            checker: TypeChecker {
                core_types,
                solver,
                available_generics: HashMap::new(),
                name_to_complex_type: HashMap::new(),
                complex_types: IndexVec::new(),
                name_to_trait: HashMap::new(),
                traits: IndexVec::new(),
                vtables: IndexVec::new(),
                local_functions: IndexVec::new(),
            },
        }
    }
}

#[derive(Debug, Clone)]
enum ValueOrFunc {
    Value(ir::Value),
    Values(Vec<ir::Value>),
    FuncRef(ir::FuncRef),
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
    fn as_ir_type(&self, solver: &TypeSolver, isa: &dyn TargetIsa) -> ir::Type;
}

impl AsIrType for TypeInfoRef {
    fn as_ir_type(&self, solver: &TypeSolver, isa: &dyn TargetIsa) -> ir::Type {
        match solver.get_info(*self) {
            TypeInfo::Unknown(fallback) => fallback.map(|ty| ty.as_ir_type(solver, isa)).unwrap_or(ir::types::INVALID),
            TypeInfo::Primitive(primitive_type) => match primitive_type {
                PrimitiveType::Any => unimplemented!(),
                PrimitiveType::ISize | PrimitiveType::USize | PrimitiveType::String | PrimitiveType::Component => isa.pointer_type(),
                PrimitiveType::I64 | PrimitiveType::U64 => ir::types::I64,
                PrimitiveType::I32 | PrimitiveType::U32 => ir::types::I32,
                PrimitiveType::I16 | PrimitiveType::U16 => ir::types::I16,
                PrimitiveType::I8 | PrimitiveType::U8 | PrimitiveType::Boolean => ir::types::I8,
                PrimitiveType::Float => ir::types::F32,
                PrimitiveType::Void => unimplemented!(),
                PrimitiveType::Null => unimplemented!(),
            },
            TypeInfo::Func(..) | TypeInfo::Trait(..) | TypeInfo::Complex(..) | TypeInfo::Array(..) => isa.pointer_type(),
            TypeInfo::Ref(_) => unreachable!(),
        }
    }
}

impl CompileTypedAST<ValueOrFunc> for StmtRef {
    fn compile(self, ast: &TypedAST, checker: &TypeChecker, compiler: &mut Compiler, fn_builder: &mut FunctionBuilder) -> CompileResult<ValueOrFunc> {
        match &ast[self] {
            Stmt::Expr(expr_ref) => expr_ref.compile(ast, checker, compiler, fn_builder),
            Stmt::VariableDecl { name, value } => {
                let var = fn_builder.declare_var(ast[*value].ty.as_ir_type(&checker.solver, compiler.codegen.module.isa()));
                let value = value.compile(ast, checker, compiler, fn_builder)?;

                if let ValueOrFunc::Value(value) = value {
                    compiler.variables.insert(name.clone(), var);

                    fn_builder.def_var(var, value);
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

            if let TypeInfo::Complex(..) = stmt
                && let ValueOrFunc::Value(ptr) = value
            {
                let mut values = Vec::new();

                let hash = checker.solver.hash_of(ast[expr].ty);

                for (field, _) in compiler.complex_types[&hash].variants[ComplexTypeVariantRef::new(0)].fields.values() {
                    values.push(fn_builder.ins().load(field.ty, ir::MemFlags::trusted(), ptr, field.offset));
                }

                ValueOrFunc::Values(values)
            } else if matches!(stmt, TypeInfo::Primitive(PrimitiveType::String))
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
                &LiteralExpr::Integer(value) => fn_builder
                    .ins()
                    .iconst(ast[self].ty.as_ir_type(&checker.solver, compiler.codegen.module.isa()), value),
                &LiteralExpr::Float(value) => fn_builder.ins().f32const(value),
                LiteralExpr::String(value) => {
                    let len = value.len();

                    compiler.codegen.data_desc.define(value.bytes().collect());

                    let id = compiler.codegen.module.declare_anonymous_data(true, false).unwrap();

                    compiler.codegen.module.define_data(id, &compiler.codegen.data_desc).unwrap();
                    compiler.codegen.data_desc.clear();
                    compiler.codegen.module.finalize_definitions().unwrap();

                    let data_id = compiler.codegen.module.declare_data_in_func(id, fn_builder.func);

                    let ptr = fn_builder.ins().global_value(compiler.codegen.module.isa().pointer_type(), data_id);
                    let size = fn_builder.ins().iconst(compiler.codegen.module.isa().pointer_type(), len.cast_signed() as i64);

                    FatPtr::new(compiler.codegen.module.isa(), fn_builder, ptr, size)
                }
                &LiteralExpr::Boolean(value) => fn_builder.ins().iconst(ir::types::I8, i64::from(value)),
            })),
            &Expr::If { condition, block, else_block } => {
                if let ValueOrFunc::Value(cond_result) = condition.compile(ast, checker, compiler, fn_builder)? {
                    let then_block = fn_builder.create_block();
                    let after_block = fn_builder.create_block();

                    let returning_param = if let Some(final_stmt) = &ast[block].value.expr {
                        let ty = ast[*final_stmt].ty.as_ir_type(&checker.solver, compiler.codegen.module.isa());

                        Some(fn_builder.append_block_param(after_block, ty))
                    } else {
                        None
                    };

                    let returned = if let Some(otherwise) = else_block {
                        let else_block = fn_builder.create_block();

                        fn_builder.ins().brif(cond_result, then_block, &[], else_block, &[]);

                        fn_builder.switch_to_block(then_block);
                        fn_builder.seal_block(then_block);

                        let returned = block.compile(ast, checker, compiler, fn_builder)?;

                        if let ValueOrFunc::Value(returned) = returned {
                            fn_builder.ins().jump(after_block, &[ir::BlockArg::Value(returned)]);
                        } else {
                            fn_builder.ins().jump(after_block, &[]);
                        }

                        fn_builder.switch_to_block(else_block);
                        fn_builder.seal_block(else_block);

                        otherwise.compile(ast, checker, compiler, fn_builder)?
                    } else {
                        fn_builder.ins().brif(cond_result, then_block, &[], after_block, &[]);

                        fn_builder.switch_to_block(then_block);
                        fn_builder.seal_block(then_block);

                        block.compile(ast, checker, compiler, fn_builder)?
                    };

                    if let ValueOrFunc::Value(returned) = returned {
                        fn_builder.ins().jump(after_block, &[ir::BlockArg::Value(returned)]);
                    } else {
                        fn_builder.ins().jump(after_block, &[]);
                    }

                    fn_builder.switch_to_block(after_block);
                    fn_builder.seal_block(after_block);

                    returning_param.map_or(Ok(ValueOrFunc::Nothing), |returning_param| Ok(ValueOrFunc::Value(returning_param)))
                } else {
                    Ok(ValueOrFunc::Nothing)
                }
            }
            Expr::Block(block_ref) => block_ref.compile(ast, checker, compiler, fn_builder),
            Expr::Var(name) => {
                if let Some(value) = compiler.assign_ref.take() {
                    let value = value.compile(ast, checker, compiler, fn_builder)?;

                    if let (Some(v), ValueOrFunc::Value(value)) = (compiler.variables.get(name), value) {
                        fn_builder.def_var(*v, value);

                        Ok(ValueOrFunc::Nothing)
                    } else {
                        unimplemented!()
                    }
                } else if let Some(v) = compiler.get(name) {
                    Ok(v)
                } else if let Some(&func) = compiler.globals.get(name) {
                    let func = compiler.codegen.module.declare_func_in_func(func, fn_builder.func);

                    compiler.values.insert(name.clone(), ValueOrFunc::FuncRef(func));

                    Ok(ValueOrFunc::FuncRef(func))
                } else if let Some(v) = compiler.variables.get(name) {
                    Ok(ValueOrFunc::Value(fn_builder.use_var(*v)))
                } else {
                    unimplemented!("there's no anything for {name}")
                }
            }
            Expr::Access { target, field } => {
                let v = target.compile(ast, checker, compiler, fn_builder)?;

                if let ValueOrFunc::Value(v) = v {
                    if let TypeInfo::Complex(ty, ..) = checker.solver.get_info2(ast[*target].ty) {
                        let hash = checker.solver.hash_of(ast[*target].ty);

                        if let Some(assign) = compiler.assign_ref.take() {
                            let value = assign.compile(ast, checker, compiler, fn_builder)?;

                            if let ValueOrFunc::Value(value) = value {
                                fn_builder.ins().store(
                                    ir::MemFlags::trusted(),
                                    value,
                                    v,
                                    compiler.complex_types[&hash].variants[ComplexTypeVariantRef::new(0)].fields[*field].0.offset,
                                );

                                return Ok(ValueOrFunc::Nothing);
                            }

                            unimplemented!()
                        } else {
                            let (field, _) = &compiler.complex_types[&hash].variants[ComplexTypeVariantRef::new(0)].fields[*field];

                            Ok(ValueOrFunc::Value(fn_builder.ins().load(field.ty, ir::MemFlags::trusted(), v, field.offset)))
                        }
                    } else {
                        unimplemented!()
                    }
                } else {
                    unimplemented!()
                }
            }
            &Expr::VTableAccess { target, func } => {
                let target = target.compile(ast, checker, compiler, fn_builder)?;

                match func {
                    VFunc::Known(vtable_ref, vfunc_ref) => {
                        compiler.this.replace(target);

                        return Ok(ValueOrFunc::FuncRef(compiler.vtables[vtable_ref][vfunc_ref].1));
                    }
                    VFunc::Unknown(_, trait_func_ref) => {
                        if let ValueOrFunc::Value(fat_ptr) = target {
                            let value = FatPtr::get_ptr(compiler.codegen.module.isa(), fn_builder, fat_ptr);

                            let vtable_ptr = FatPtr::get_metadata(compiler.codegen.module.isa(), fn_builder, fat_ptr);
                            let vtable_func =
                                VTablePtr::get_func_ptr(compiler.codegen.module.isa(), fn_builder, vtable_ptr, trait_func_ref.index().try_into()?);

                            compiler.this.replace(ValueOrFunc::Value(value));

                            return Ok(ValueOrFunc::Value(vtable_func));
                        }
                    }
                }

                Ok(ValueOrFunc::Nothing)
            }
            &Expr::Index { target, index } => {
                let target = target.compile(ast, checker, compiler, fn_builder)?;
                let index = index.compile(ast, checker, compiler, fn_builder)?;

                if let (ValueOrFunc::Value(array), ValueOrFunc::Value(index)) = (target, index) {
                    let element_type = ast[self].ty.as_ir_type(&checker.solver, compiler.codegen.module.isa());
                    let size = fn_builder
                        .ins()
                        .iconst(compiler.codegen.module.isa().pointer_type(), i64::from(element_type.bytes()));

                    let offset = fn_builder.ins().imul(size, index);
                    let ptr = FatPtr::get_ptr(compiler.codegen.module.isa(), fn_builder, array);
                    let ptr = fn_builder.ins().iadd(ptr, offset);

                    if let Some(assign) = compiler.assign_ref.take() {
                        if let ValueOrFunc::Value(value) = assign.compile(ast, checker, compiler, fn_builder)? {
                            fn_builder.ins().store(ir::MemFlags::trusted(), value, ptr, 0);

                            return Ok(ValueOrFunc::Nothing);
                        }
                    } else {
                        return Ok(ValueOrFunc::Value(fn_builder.ins().load(element_type, ir::MemFlags::trusted(), ptr, 0)));
                    }
                }

                todo!()
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
                let mut values = Vec::with_capacity(expr_refs.len());

                for expr_ref in expr_refs {
                    if let ValueOrFunc::Value(value) = expr_ref.compile(ast, checker, compiler, fn_builder)? {
                        values.push(value);
                    }
                }

                Ok(ValueOrFunc::Value(
                    Array {
                        element: ast[self].ty.as_ir_type(&checker.solver, compiler.codegen.module.isa()),
                    }
                    .instance(compiler.codegen.module.isa(), fn_builder, values),
                ))
            }
            &Expr::Binary {
                operator,
                lhs: lhs_ref,
                rhs: rhs_ref,
            } => {
                if matches!(operator, Operator::Assign) {
                    let old = compiler.assign_ref.replace(rhs_ref);

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

                    if let (ValueOrFunc::Value(lhs), ValueOrFunc::Value(rhs)) = (lhs, rhs) {
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
                        unimplemented!()
                    }
                }
            }
            Expr::Call { func, args } => {
                if let Some(TypeInfo::Func(arg_types, returns)) = checker.solver.get_maybe_info(ast[*func].ty) {
                    let v = func.compile(ast, checker, compiler, fn_builder)?;
                    let mut arg_values = Vec::new();

                    if let Some(ValueOrFunc::Value(this)) = compiler.this.take() {
                        arg_values.push(this);
                    }

                    for arg in args {
                        let value = arg.compile(ast, checker, compiler, fn_builder)?;

                        match value {
                            ValueOrFunc::Value(value) => arg_values.push(value),
                            ValueOrFunc::Nothing => (),
                            _ => panic!("received incorrect value for argument: {arg:?} {value:?}"),
                        }
                    }

                    let value = if let ValueOrFunc::FuncRef(func) = v {
                        let result = fn_builder.ins().call(func, &arg_values);

                        match fn_builder.inst_results(result) {
                            [] => ValueOrFunc::Nothing,
                            &[value] => ValueOrFunc::Value(value),
                            values => ValueOrFunc::Values(values.to_vec()),
                        }
                    } else if let ValueOrFunc::Value(func_addr) = v {
                        let mut signature = compiler.codegen.module.make_signature();

                        for arg in arg_types {
                            signature
                                .params
                                .push(AbiParam::new(arg.as_ir_type(&checker.solver, compiler.codegen.module.isa())));
                        }

                        if let Some(TypeInfo::Complex(structure, ..)) = checker.solver.get_maybe_info(*returns) {
                            let hash = checker.solver.hash_of(*returns);

                            for (field, _) in compiler.complex_types[&hash].variants[ComplexTypeVariantRef::new(0)].fields.values() {
                                signature.returns.push(AbiParam::new(field.ty));
                            }
                        } else if returns == &checker.core_types.string {
                            signature.returns.push(AbiParam::new(compiler.codegen.module.isa().pointer_type()));
                            signature.returns.push(AbiParam::new(compiler.codegen.module.isa().pointer_type()));
                        } else if returns != &checker.core_types.void {
                            signature
                                .returns
                                .push(AbiParam::new(returns.as_ir_type(&checker.solver, compiler.codegen.module.isa())));
                        }

                        let sig = fn_builder.import_signature(signature);

                        let result = fn_builder.ins().call_indirect(sig, func_addr, &arg_values);

                        match fn_builder.inst_results(result) {
                            [] => ValueOrFunc::Nothing,
                            &[value] => ValueOrFunc::Value(value),
                            values => ValueOrFunc::Values(values.to_vec()),
                        }
                    } else {
                        ValueOrFunc::Nothing
                    };

                    if let Some(TypeInfo::Complex(structure, ..)) = checker.solver.get_maybe_info(*returns)
                        && let ValueOrFunc::Values(values) = value
                    {
                        let hash = checker.solver.hash_of(*returns);

                        Ok(ValueOrFunc::Value(compiler.complex_types[&hash].instance(
                            ComplexTypeVariantRef::new(0),
                            compiler.codegen.module.isa(),
                            fn_builder,
                            values,
                        )))
                    } else if returns == &checker.core_types.string
                        && let ValueOrFunc::Values(values) = value
                    {
                        Ok(ValueOrFunc::Value(FatPtr::new(compiler.codegen.module.isa(), fn_builder, values[0], values[1])))
                    } else {
                        Ok(value)
                    }
                } else {
                    panic!("dada");
                }
            }
            Expr::Closure { args, body } => {
                let values = mem::take(&mut compiler.values);
                let mut signature = compiler.codegen.module.make_signature();

                if let TypeInfo::Func(args, returns) = checker.solver.get_info(ast[self].ty) {
                    for arg in args {
                        if arg != &checker.core_types.void {
                            signature
                                .params
                                .push(AbiParam::new(arg.as_ir_type(&checker.solver, compiler.codegen.module.isa())));
                        }
                    }

                    if returns != &checker.core_types.void {
                        signature
                            .returns
                            .push(AbiParam::new(returns.as_ir_type(&checker.solver, compiler.codegen.module.isa())));
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

                for (index, name) in args.iter().enumerate() {
                    let value = closure_fn_builder.block_params(entry_block)[index];
                    let ty = closure_fn_builder.func.signature.params[index].value_type;

                    let var = closure_fn_builder.declare_var(ty);

                    closure_fn_builder.def_var(var, value);

                    compiler.variables.insert(name.clone(), var);
                }

                let result = body.compile(ast, checker, compiler, &mut closure_fn_builder).unwrap();

                match result {
                    ValueOrFunc::Value(value) => closure_fn_builder.ins().return_(&[value]),
                    ValueOrFunc::Values(values) => closure_fn_builder.ins().return_(&values),
                    _ => closure_fn_builder.ins().return_(&[]),
                };

                compiler.codegen.module.define_function(func_id, &mut ctx).unwrap();
                compiler.codegen.module.clear_context(&mut ctx);
                compiler.codegen.module.finalize_definitions().unwrap();

                for name in args {
                    compiler.variables.shift_remove(name);
                }

                compiler.values = values;

                let func = compiler.codegen.module.declare_func_in_func(func_id, fn_builder.func);
                let ptr = fn_builder.ins().func_addr(compiler.codegen.module.isa().pointer_type(), func);

                Ok(ValueOrFunc::Value(ptr))
            }
            Expr::Construct { ty, fields } => {
                if checker.solver.get_info2(*ty).is_struct() {
                    let mut values = Vec::new();
                    let hash = checker.solver.hash_of(*ty);

                    for (field_ref, (field, _)) in compiler.complex_types[&hash].variants[ComplexTypeVariantRef::new(0)].fields.clone().into_iter() {
                        if let Some(property) = fields.iter().find(|property| property.0 == field_ref) {
                            let got = match property.2 {
                                Some(expr) => expr.compile(ast, checker, compiler, fn_builder)?,
                                None => match &field.default_value {
                                    Some(value) => compile_constant(value, &mut compiler.codegen.module, &mut compiler.codegen.data_desc, fn_builder)
                                        .map_or(ValueOrFunc::Nothing, ValueOrFunc::Value),
                                    None => panic!("can't compile {field:?}: no value"),
                                },
                            };

                            if let ValueOrFunc::Value(v) = got {
                                assert_eq!(field.ty, fn_builder.func.dfg.value_type(v), "got incorrect type for ...");

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
                            }
                        }
                    }

                    return Ok(ValueOrFunc::Value(compiler.complex_types[&hash].instance(
                        ComplexTypeVariantRef::new(0),
                        compiler.codegen.module.isa(),
                        fn_builder,
                        values,
                    )));
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
                                let got = /* match property.1 {
                                    Some(expr) =>  */property.2.compile(ast, checker, compiler, fn_builder)?/* ,
                                    None => match &field.default_value {
                                        Some(value) => compile_constant(value, &mut compiler.codegen.module, &mut compiler.codegen.data_desc, fn_builder)
                                            .map_or(ValueOrFunc::Nothing, ValueOrFunc::Value),
                                        None => panic!("can't compile {field:?}: no value"),
                                    },
                                } */;

                                if let ValueOrFunc::Value(v) = got {
                                    assert_eq!(field.ty, fn_builder.func.dfg.value_type(v), "got incorrect type for ...");

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
                                }
                            }
                        }
                    }

                    let metadata = fn_builder.ins().iconst(compiler.codegen.module.isa().pointer_type(), *variant as i64);
                    let enum_value = if compiler.complex_types[&hash].size == 0 {
                        metadata
                    } else {
                        compiler.complex_types[&hash].instance(ComplexTypeVariantRef::new(*variant), compiler.codegen.module.isa(), fn_builder, values)
                    };

                    return Ok(ValueOrFunc::Value(FatPtr::new(compiler.codegen.module.isa(), fn_builder, enum_value, metadata)));
                }

                Ok(ValueOrFunc::Nothing)
            }
            Expr::ConstructComponent { .. } => todo!(),
            Expr::IsPattern { target, pattern } => {
                fn compile_pattern(
                    target: ir::Value,
                    target_ty: TypeInfoRef,
                    pattern: &IsPattern,
                    ast: &TypedAST,
                    checker: &TypeChecker,
                    compiler: &mut Compiler,
                    fn_builder: &mut FunctionBuilder,
                ) -> CompileResult<ValueOrFunc> {
                    match pattern {
                        &IsPattern::Literal(expr_ref) => {
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
                        }
                        IsPattern::EnumVariant {
                            target: target_ty,
                            target_args,
                            variant,
                            values,
                        } => {
                            let metadata = FatPtr::get_metadata(compiler.codegen().module.isa(), fn_builder, target);
                            let expected = fn_builder.ins().iconst(compiler.codegen().module.isa().pointer_type(), variant.index() as i64);

                            let result = fn_builder.ins().icmp(IntCC::Equal, metadata, expected);

                            if let Some(values) = values {
                                let target = FatPtr::get_ptr(compiler.codegen().module.isa(), fn_builder, target);
                                let hash = {
                                    let mut state = DefaultHasher::new();

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

                                    let value = fn_builder.ins().load(field.ty, ir::MemFlags::trusted(), target, field.offset);

                                    if let Some(pattern) = pattern {
                                        compile_pattern(value, *field_ty, pattern, ast, checker, compiler, fn_builder)?;
                                    } else {
                                        let var = fn_builder.declare_var(field.ty);

                                        fn_builder.def_var(var, value);

                                        compiler.variables.insert(name.into(), var);
                                    }
                                }

                                fn_builder.ins().jump(after_block, &[]);

                                fn_builder.switch_to_block(after_block);
                                fn_builder.seal_block(after_block);
                            }

                            Ok(ValueOrFunc::Value(result))
                        }
                        IsPattern::TypeName { ty, name } => todo!(),
                    }
                }

                let target_ty = ast[*target].ty;
                let target = target.compile(ast, checker, compiler, fn_builder)?;

                if let ValueOrFunc::Value(target) = target {
                    compile_pattern(target, target_ty, pattern, ast, checker, compiler, fn_builder)
                } else {
                    Ok(ValueOrFunc::Nothing)
                }
            }
            Expr::TypeIndex { .. } => todo!(),
            Expr::Nothing => Ok(ValueOrFunc::Nothing),
        }
    }
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

fn do_println_str(value: &&str) {
    println!("{value}");
}
