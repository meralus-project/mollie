#![allow(clippy::result_large_err)]

use std::{fmt, num::TryFromIntError, sync::Arc};

pub use cranelift;
use cranelift::{
    codegen::{Context, ir},
    jit::{JITBuilder, JITModule},
    module::{DataDescription, DataId, FuncId, Linkage, Module, ModuleResult, default_libcall_names},
    native,
    prelude::{AbiParam, Configurable, FunctionBuilder, FunctionBuilderContext, InstBuilder, Signature, isa::TargetIsa, settings, types},
};
pub use indexmap::IndexMap;
use indexmap::IndexSet;
use mollie_ir::{Array, FatPtr, VTablePtr};
use mollie_lexer::{Lexer, Token};
use mollie_parser::{Expr, Parser, Stmt, parse_statements_until};
use mollie_shared::{Positioned, Span};
use mollie_typing::{ArrayType, ComplexType, FunctionType, PrimitiveType, Trait, Type, TypeKind, TypeVariant};

pub use self::error::{CompileError, CompileResult, TypeError, TypeResult};

mod error;
mod statement;
mod ty;

#[derive(Debug)]
pub struct Variable {
    pub id: usize,
    pub ty: usize,
}

pub type VTable = IndexMap<Option<usize>, (DataId, IndexMap<String, (Type, (ir::SigRef, ir::FuncRef, FuncId))>)>;

pub struct JitCompiler {
    pub module: JITModule,
    pub data_desc: DataDescription,
}

impl fmt::Debug for JitCompiler {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("JitCompiler").field("data_desc", &self.data_desc).finish_non_exhaustive()
    }
}

impl JitCompiler {
    fn jit_builder(symbols: Vec<(&'static str, *const u8)>, isa: Arc<dyn TargetIsa>) -> JITBuilder {
        let mut builder = JITBuilder::with_isa(isa, default_libcall_names());

        builder.symbol("println", do_println as *const u8);
        builder.symbol("println_bool", do_println_bool as *const u8);
        builder.symbol("println_addr", do_println_addr as *const u8);
        builder.symbol("println_str", do_println_str as *const u8);
        builder.symbol("println_float", do_println_f32 as *const u8);

        for (name, ptr) in symbols {
            builder.symbol(name, ptr);
        }

        builder
    }

    fn new(symbols: Vec<(&'static str, *const u8)>, flags: settings::Flags) -> Self {
        let isa = native::builder().unwrap();
        let isa = isa.finish(flags).unwrap();

        let module = JITModule::new(Self::jit_builder(symbols, isa));

        Self {
            module,
            data_desc: DataDescription::new(),
        }
    }
}

#[derive(Debug)]
pub struct Compiler {
    pub traits: IndexMap<String, Trait>,
    pub name_to_type: IndexMap<String, usize>,
    pub types: IndexSet<Type>,
    pub impls: IndexMap<TypeVariant, Vec<usize>>,
    pub vtables: IndexMap<TypeVariant, VTable>,
    pub frames: Vec<IndexMap<String, Variable>>,

    pub generics: Vec<Type>,
    pub assign: Option<Positioned<Expr>>,
    pub this_ty: Option<Type>,
    pub this: Option<ValueOrFunc>,
    pub infer: Option<Type>,
    pub infer_val: Option<ValueOrFunc>,
    pub values: IndexMap<String, ValueOrFunc>,
    pub variables: IndexMap<String, cranelift::prelude::Variable>,
    pub globals: IndexMap<String, FuncId>,
    pub func_names: IndexMap<FuncId, String>,

    pub jit: JitCompiler,
}

impl Default for Compiler {
    fn default() -> Self {
        Self::with_symbols(Vec::new())
    }
}

impl Compiler {
    fn get_property<T: AsRef<str>>(&self, mut ty: Type, property_name: T) -> TypeResult<Type> {
        let property_name = property_name.as_ref();

        let mut result = match ty.variant {
            TypeVariant::This | TypeVariant::Generic(_) => unreachable!(),
            TypeVariant::Primitive(ty) => unimplemented!("{ty} doesn't have property called {property_name}"),
            TypeVariant::Trait(t) => {
                return self.traits[t]
                    .functions
                    .iter()
                    .find(|func| func.name == property_name)
                    .map(|func| Type {
                        variant: TypeVariant::complex(ComplexType::Function(FunctionType {
                            is_native: false,
                            this: if func.this { Some(ty.clone()) } else { None },
                            args: func.args.clone(),
                            returns: Box::new(func.returns.clone()),
                        })),
                        applied_generics: ty.applied_generics,
                        declared_at: self.traits[t].declared_at,
                    })
                    .ok_or_else(|| TypeError::PropertyNotFound {
                        ty: Box::new(TypeKind::Struct),
                        ty_name: None,
                        property: property_name.to_string(),
                    });
            }
            TypeVariant::Complex(ref complex_type) => match &**complex_type {
                ComplexType::Component(component) => {
                    if property_name == "children" {
                        let element = component
                            .children
                            .as_ref()
                            .ok_or_else(|| TypeError::PropertyNotFound {
                                ty: Box::new(TypeKind::Component),
                                ty_name: None,
                                property: property_name.to_string(),
                            })?
                            .clone();

                        if let Some(array) = element.variant.as_array() {
                            ty.applied_generics.push(array.element.clone());
                        }

                        return Ok(Type {
                            variant: element.variant,
                            applied_generics: ty.applied_generics,
                            declared_at: None,
                        });
                    }

                    component
                        .properties
                        .iter()
                        .find(|(name, ..)| name == property_name)
                        .map(|(.., v)| v.clone().resolve_type(&ty.applied_generics))
                        .ok_or_else(|| TypeError::PropertyNotFound {
                            ty: Box::new(TypeKind::Component),
                            ty_name: None,
                            property: property_name.to_string(),
                        })
                }
                ComplexType::Struct(structure) => structure
                    .properties
                    .iter()
                    .find(|(name, _)| name == property_name)
                    .map(|(.., v)| v.clone().resolve_type(&ty.applied_generics))
                    .ok_or_else(|| TypeError::PropertyNotFound {
                        ty: Box::new(TypeKind::Struct),
                        ty_name: None,
                        property: property_name.to_string(),
                    }),
                ComplexType::TraitInstance(ty, trait_index) => self.traits[*trait_index]
                    .functions
                    .iter()
                    .find(|f| f.name == property_name)
                    .map(|f| {
                        TypeVariant::complex(ComplexType::Function(FunctionType {
                            is_native: false,
                            this: if f.this { Some(ty.clone()) } else { None },
                            args: f.args.clone(),
                            returns: Box::new(f.returns.clone()),
                        }))
                        .into()
                    })
                    .ok_or_else(|| TypeError::PropertyNotFound {
                        ty: Box::new(TypeKind::Struct),
                        ty_name: None,
                        property: property_name.to_string(),
                    }),
                _ => unimplemented!("{} cannot be indexed by {}", ty.clone().resolve_type(&ty.applied_generics), property_name),
            },
            TypeVariant::Ref { ty, mutable } => self
                .get_property(*ty, property_name)
                .map(|ty| TypeVariant::Ref { ty: Box::new(ty), mutable }.into()),
        }?;

        result.applied_generics.extend(ty.applied_generics);

        Ok(result)
    }

    fn import_fn<T: IntoIterator<Item = ir::Type>>(&mut self, name: &str, params: T) -> ModuleResult<FuncId> {
        let mut signature = self.jit.module.make_signature();

        signature.params.extend(params.into_iter().map(AbiParam::new));

        let id = self.jit.module.declare_function(name, Linkage::Import, &signature)?;

        self.func_names.insert(id, name.to_owned());
        self.globals.insert(name.to_owned(), id);

        Ok(id)
    }

    /// Gets a pointer to the compiled function with the specified `name` and
    /// `transmute`s it to `T`.
    ///
    /// # Safety
    ///
    /// `T` must be a function type and be the same size as pointers, otherwise
    /// you will get undefined behavior.
    pub unsafe fn get_func<T>(&self, name: impl AsRef<str>) -> Option<T> {
        debug_assert_eq!(std::mem::size_of::<T>(), std::mem::size_of::<*const u8>());

        self.globals.get(name.as_ref()).map(|&func_id| {
            let code = self.jit.module.get_finalized_function(func_id);

            unsafe { std::mem::transmute_copy::<std::mem::ManuallyDrop<*const u8>, T>(&std::mem::ManuallyDrop::new(code)) }
        })
    }
}

#[derive(Debug)]
pub enum VTableCreationError {
    TooMuchFunctions,
    ModuleError(cranelift::module::ModuleError),
}

impl From<TryFromIntError> for VTableCreationError {
    fn from(_: TryFromIntError) -> Self {
        Self::TooMuchFunctions
    }
}

impl From<cranelift::module::ModuleError> for VTableCreationError {
    fn from(value: cranelift::module::ModuleError) -> Self {
        Self::ModuleError(value)
    }
}

impl std::error::Error for VTableCreationError {}

impl fmt::Display for VTableCreationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TooMuchFunctions => f.write_str("number of functions exceeds `(u32::MAX - 1) / size_of::<usize>()`"),
            Self::ModuleError(error) => error.fmt(f),
        }
    }
}

impl FuncCompiler<'_, '_> {
    /// Creates the main vtable for the type with the specified `type_idx`,
    /// containing the functions specified in `functions`. Think of this vtable
    /// as `impl T { ... }`.
    ///
    /// However, you cannot create multiple main vtables, as they will overwrite
    /// each other.
    ///
    /// # Errors
    ///
    /// Returns [`VTableCreationError::TooMuchFunctions`] if the number of
    /// functions exceeds `(u32::MAX - 1) / size_of::<usize>()`, where `usize`
    /// refers to the compilation target, not the host.
    ///
    /// Returns [`VTableCreationError::ModuleError`] if the vtable declaration
    /// failed at any stage.
    pub fn create_fallback_vtable<K: Into<String>, I: IntoIterator<Item = (K, (Type, (ir::SigRef, ir::FuncRef, FuncId)))>>(
        &mut self,
        type_idx: usize,
        functions: I,
    ) -> Result<(), VTableCreationError> {
        let ty = self.compiler.types[type_idx].variant.clone();
        let functions: IndexMap<String, (Type, (ir::SigRef, ir::FuncRef, FuncId))> = functions.into_iter().map(|(name, func)| (name.into(), func)).collect();
        let size_t = self.compiler.jit.module.isa().pointer_bytes();
        let data_size = usize::from(size_t) * (functions.len() + 1);
        let mut data = vec![0; data_size];

        data[0..usize::from(size_t)].copy_from_slice(&match self.compiler.jit.module.isa().endianness() {
            ir::Endianness::Little => type_idx.to_le_bytes(),
            ir::Endianness::Big => type_idx.to_be_bytes(),
        });

        self.compiler.jit.data_desc.define(data.into_boxed_slice());

        for (i, (_, (_, _, func_id))) in functions.values().enumerate() {
            let func_ref = self.compiler.jit.module.declare_func_in_data(*func_id, &mut self.compiler.jit.data_desc);

            self.compiler
                .jit
                .data_desc
                .write_function_addr(u32::from(size_t) * (u32::try_from(i)? + 1), func_ref);
        }

        let id = self.compiler.jit.module.declare_anonymous_data(false, false)?;

        self.compiler.jit.module.define_data(id, &self.compiler.jit.data_desc)?;
        self.compiler.jit.data_desc.clear();
        self.compiler.jit.module.finalize_definitions()?;

        let functions = (id, functions);

        self.compiler.vtables.insert(ty, VTable::from_iter([(None, functions)]));

        Ok(())
    }

    /// Declares a function pointing to `ext_name` with a possible `self`
    /// argument, other arguments specified in `args`, and a return type
    /// specified in `returns`.
    ///
    /// Returns the function type, a reference to the signature, a reference to
    /// the function, and the function ID.
    ///
    /// # Errors
    ///
    /// Returns [`ModuleError::IncompatibleDeclaration`] if `ext_name` is not a
    /// function, or [`ModuleError::IncompatibleSignature`] if the signature of
    /// the external function does not match the specified one.
    ///
    /// [`ModuleError::IncompatibleDeclaration`]: cranelift::module::ModuleError::IncompatibleDeclaration
    /// [`ModuleError::IncompatibleSignature`]: cranelift::module::ModuleError::IncompatibleSignature
    pub fn add_native_fn<N: AsRef<str>, T: IntoIterator<Item = TypeVariant>, R: Into<Type>>(
        &mut self,
        ext_name: N,
        this: Option<TypeVariant>,
        args: T,
        returns: R,
    ) -> ModuleResult<(Type, (ir::SigRef, ir::FuncRef, FuncId))> {
        let (func_ty, sig) = TypeVariant::function_ir(self.compiler.jit.module.isa(), this, args, returns);
        let func_id = self.compiler.jit.module.declare_function(ext_name.as_ref(), Linkage::Import, &sig)?;
        let sig = self.fn_builder.import_signature(sig);
        let func = self.compiler.jit.module.declare_func_in_func(func_id, self.fn_builder.func);

        Ok((func_ty.into(), (sig, func, func_id)))
    }
}

pub struct FuncCompilerBuilder<'a> {
    pub compiler: &'a mut Compiler,
    pub ctx: Context,
    pub fn_builder_ctx: FunctionBuilderContext,
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
        let signature = self.compiler.jit.module.make_signature();

        Self::provide_with_signature(self, signature)
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
    pub fn provide_with_signature(&mut self, signature: Signature) -> ModuleResult<FuncCompiler<'_, '_>> {
        let ctx = &mut self.ctx;
        let fn_builder_ctx = &mut self.fn_builder_ctx;

        self.compiler.import_fn("println", [types::I64])?;
        self.compiler.import_fn("println_str", [types::I64])?;
        self.compiler.import_fn("println_bool", [types::I8])?;
        self.compiler.import_fn("println_addr", [types::I64])?;
        self.compiler.import_fn("println_float", [types::F32])?;

        let get_type_idx_id = {
            let mut get_type_idx_sig = self.compiler.jit.module.make_signature();

            get_type_idx_sig.params.push(AbiParam::new(self.compiler.jit.module.isa().pointer_type()));
            get_type_idx_sig.returns.push(AbiParam::new(self.compiler.jit.module.isa().pointer_type()));

            let func = self.compiler.jit.module.declare_function("get_type_idx", Linkage::Local, &get_type_idx_sig)?;

            ctx.func.signature = get_type_idx_sig;

            let mut fn_builder_ctx = FunctionBuilderContext::new();
            let mut fn_builder = FunctionBuilder::new(&mut ctx.func, &mut fn_builder_ctx);

            let entry_block = fn_builder.create_block();

            fn_builder.append_block_params_for_function_params(entry_block);
            fn_builder.switch_to_block(entry_block);
            fn_builder.seal_block(entry_block);

            let fat_ptr = fn_builder.block_params(entry_block)[0];
            let vtable_ptr = FatPtr::get_metadata(self.compiler.jit.module.isa(), &mut fn_builder, fat_ptr);
            let type_idx = VTablePtr::get_type_idx(self.compiler.jit.module.isa(), &mut fn_builder, vtable_ptr);

            fn_builder.ins().return_(&[type_idx]);

            self.compiler.jit.module.define_function(func, ctx)?;
            self.compiler.jit.module.clear_context(ctx);

            func
        };

        let get_size_id = {
            let mut get_size_sig = self.compiler.jit.module.make_signature();

            get_size_sig.params.push(AbiParam::new(self.compiler.jit.module.isa().pointer_type()));
            get_size_sig.returns.push(AbiParam::new(self.compiler.jit.module.isa().pointer_type()));

            let func = self.compiler.jit.module.declare_function("get_size", Linkage::Local, &get_size_sig)?;

            ctx.func.signature = get_size_sig;

            let mut fn_builder_ctx = FunctionBuilderContext::new();
            let mut fn_builder = FunctionBuilder::new(&mut ctx.func, &mut fn_builder_ctx);

            let entry_block = fn_builder.create_block();

            fn_builder.append_block_params_for_function_params(entry_block);
            fn_builder.switch_to_block(entry_block);
            fn_builder.seal_block(entry_block);

            let fat_ptr = fn_builder.block_params(entry_block)[0];
            let size = FatPtr::get_metadata(self.compiler.jit.module.isa(), &mut fn_builder, fat_ptr);

            fn_builder.ins().return_(&[size]);

            self.compiler.jit.module.define_function(func, ctx)?;
            self.compiler.jit.module.clear_context(ctx);

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
        })
    }
}

pub struct FuncCompiler<'a, 'b> {
    pub compiler: &'a mut Compiler,
    pub fn_builder: FunctionBuilder<'b>,
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

        self.compile_program(program)
    }

    pub fn set_main_signature(&mut self, signature: Signature) {
        self.fn_builder.func.signature = signature;
    }

    /// # Errors
    ///
    /// Returns `CompileError` if program compilation fails.
    pub fn compile_program(&mut self, (statements, returned): (Vec<Positioned<Stmt>>, Option<Positioned<Stmt>>)) -> CompileResult<FuncId> {
        for statement in statements {
            self.compiler.compile(&mut self.fn_builder, statement)?;
        }

        if let Some(statement) = returned {
            self.compiler.compile(&mut self.fn_builder, statement)?;
        }

        self.fn_builder.ins().return_(&[]);
        self.compiler
            .jit
            .module
            .declare_function("<main>", Linkage::Export, &self.fn_builder.func.signature)
            .map_err(CompileError::Module)
    }
}

impl Compiler {
    pub fn with_symbols(symbols: Vec<(&'static str, *const u8)>) -> Self {
        let mut flag_builder = settings::builder();

        unsafe {
            flag_builder.set("use_colocated_libcalls", "false").unwrap_unchecked();
            flag_builder.set("opt_level", "speed").unwrap_unchecked();
            flag_builder.set("is_pic", "false").unwrap_unchecked();
        }

        Self {
            traits: IndexMap::new(),
            types: IndexSet::new(),
            name_to_type: IndexMap::new(),
            impls: IndexMap::new(),
            vtables: IndexMap::new(),
            frames: vec![IndexMap::new()],
            generics: Vec::new(),
            assign: None,
            this_ty: None,
            this: None,
            infer: None,
            infer_val: None,
            jit: JitCompiler::new(symbols, settings::Flags::new(flag_builder)),
            values: IndexMap::new(),
            globals: IndexMap::new(),
            variables: IndexMap::new(),
            func_names: IndexMap::new(),
        }
    }

    pub fn get<T: AsRef<str>>(&self, name: T) -> Option<ValueOrFunc> {
        self.values.get(name.as_ref()).cloned()
    }

    pub fn add_type<T: Into<String>>(&mut self, name: T, ty: TypeVariant) -> usize {
        let type_idx = self.types.insert_full(ty.into()).0;

        self.name_to_type.insert(name.into(), type_idx);

        type_idx
    }

    pub fn add_declared_type<T: Into<String>>(&mut self, name: T, ty: Type) -> usize {
        let type_idx = self.types.insert_full(ty).0;

        self.name_to_type.insert(name.into(), type_idx);

        type_idx
    }

    pub fn remove_type<T: AsRef<str>>(&mut self, name: T) {
        if let Some(_idx) = self.name_to_type.shift_remove(name.as_ref()) {}
    }

    pub fn push_frame(&mut self) {
        self.frames.push(IndexMap::new());
    }

    pub fn pop_frame(&mut self) {
        self.frames.pop();
    }

    pub const fn current_frame_id(&self) -> usize {
        self.frames.len() - 1
    }

    pub fn current_frame(&self) -> &IndexMap<String, Variable> {
        let Some(frame) = self.frames.last() else { unreachable!() };

        frame
    }

    pub fn current_frame_mut(&mut self) -> &mut IndexMap<String, Variable> {
        let Some(frame) = self.frames.last_mut() else { unreachable!() };

        frame
    }

    pub fn get_var<T: AsRef<str>>(&self, name: T) -> Option<&Variable> {
        let name = name.as_ref();

        for frame in self.frames.iter().rev() {
            if let Some(var) = frame.get(name) {
                return Some(var);
            }
        }

        None
    }

    pub fn get_var_index<T: AsRef<str>>(&self, name: T) -> Option<(usize, usize)> {
        let name = name.as_ref();

        for (frame_id, frame) in self.frames.iter().enumerate().rev() {
            if let Some(var) = frame.get(name) {
                return Some((self.current_frame_id() - frame_id, var.id));
            }
        }

        None
    }

    pub fn var_ty<T: Into<String>, V: Into<Type>>(&mut self, name: T, ty: V) -> usize {
        let id = self.current_frame().len();
        let name = name.into();
        let ty = ty.into();
        let ty_idx = if let Some(idx) = self.types.get_index_of(&ty) {
            idx
        } else {
            self.types.insert_full(ty).0
        };

        self.current_frame_mut().insert(name, Variable { id, ty: ty_idx });

        id
    }

    pub fn var<T: Into<String>>(&mut self, name: T, ty_idx: usize) -> usize {
        let id = self.current_frame().len();
        let name = name.into();

        self.current_frame_mut().insert(name, Variable { id, ty: ty_idx });

        id
    }

    pub fn remove_var<T: AsRef<str>>(&mut self, name: T) {
        let name = name.as_ref();

        self.current_frame_mut().shift_remove(name);
    }

    pub fn start_compiling(&mut self) -> FuncCompilerBuilder<'_> {
        let ctx = self.jit.module.make_context();
        let fn_builder_ctx = FunctionBuilderContext::new();

        FuncCompilerBuilder {
            compiler: self,
            ctx,
            fn_builder_ctx,
        }
    }

    /// # Errors
    ///
    /// Returns `CompileError` if compilation fails. This can happen in two
    /// cases: if the referenced variable is not found or if type inference
    /// fails.
    pub fn compile<O, T: Compile<O>>(&mut self, fn_builder: &mut FunctionBuilder, value: T) -> CompileResult<O> {
        value.compile(self, fn_builder)
    }

    /// # Errors
    ///
    /// Returns `TypeError` if type inference fails. This can happen if, for
    /// example, the required type is not declared.
    pub fn get_positioned_type<T: GetType>(&mut self, value: &Positioned<T>) -> TypeResult {
        value.get_type(self)
    }

    /// # Errors
    ///
    /// Returns `TypeError` if type inference fails. This can happen if, for
    /// example, the required type is not declared.
    pub fn get_value_type<T: GetType>(&mut self, value: &T, span: Span) -> TypeResult {
        value.get_type(self, span)
    }

    /// # Errors
    ///
    /// Will throw `CompileError` if there's no type with that `name`.
    pub fn try_get_type<T: AsRef<str>>(&self, name: T) -> CompileResult<Type> {
        self.name_to_type
            .get(name.as_ref())
            .map(|&idx| self.types[idx].clone())
            .ok_or_else(|| CompileError::VariableNotFound {
                name: name.as_ref().to_string(),
            })
    }

    /// # Errors
    ///
    /// Will throw `TypeError` if there's no local with that `name`.
    pub fn get_local_type<T: AsRef<str>>(&self, name: T) -> TypeResult<Type> {
        for frame in self.frames.iter().rev() {
            if let Some(v) = frame.get(name.as_ref()) {
                return Ok(self.types[v.ty].clone());
            }
        }

        Err(TypeError::NotFound {
            ty: None,
            name: name.as_ref().to_string(),
        })
    }

    pub fn idx_of_type(&self, ty: &Type) -> Option<usize> {
        self.types.get_index_of(ty)
    }

    pub fn get_type_idx<T: AsRef<str>>(&self, name: T) -> Option<usize> {
        self.name_to_type.get(name.as_ref()).copied()
    }

    pub fn get_type<T: AsRef<str>>(&self, name: T) -> Option<&Type> {
        self.name_to_type.get(name.as_ref()).map(|&idx| &self.types[idx])
    }

    /// # Panics
    ///
    /// Will panic if there's no type with that `name`.
    pub fn get_type_unchecked<T: AsRef<str>>(&self, name: T) -> Type {
        let name = name.as_ref();

        self.get_type(name).map_or_else(|| panic!("{name} not found"), Clone::clone)
    }

    fn find_vtable_function_index_<T: AsRef<str>>(&self, vtable_index: usize, function_name: T) -> Option<(usize, Option<usize>, usize)> {
        let index = self.vtables[vtable_index]
            .iter()
            .find_map(|(i, vtable)| vtable.1.get_index_of(function_name.as_ref()).map(|index| (*i, index)));

        index.map(|index| (vtable_index, index.0, index.1))
    }

    pub fn find_vtable_function<T: AsRef<str>>(&self, ty: &TypeVariant, contains: T) -> Option<&(Type, (ir::SigRef, ir::FuncRef, FuncId))> {
        self.find_vtable_function_index(ty, contains).map(|v| &self.vtables[v.0][&v.1].1[v.2])
    }

    pub fn find_vtable_function_index<T: AsRef<str>>(&self, ty: &TypeVariant, function_name: T) -> Option<(usize, Option<usize>, usize)> {
        let ty = if let TypeVariant::Ref { ty, .. } = ty { &ty.variant } else { ty };

        self.vtables.get_index_of(ty).map_or_else(
            || {
                ty.as_array().map_or_else(
                    || {
                        ty.as_component()
                            .map_or_else(
                                || self.vtables.get_index_of(&TypeVariant::any()),
                                |_| self.vtables.get_index_of(&TypeVariant::Primitive(PrimitiveType::Component)),
                            )
                            .and_then(|v| self.find_vtable_function_index_(v, function_name.as_ref()))
                    },
                    |ty| {
                        self.vtables
                            .get_index_of(&TypeVariant::complex(ComplexType::Array(ArrayType {
                                element: ty.element.clone(),
                                size: None,
                                array: Array {
                                    element: ty.element.variant.as_ir_type(self.jit.module.isa()),
                                },
                            })))
                            .and_then(|v| self.find_vtable_function_index_(v, function_name.as_ref()))
                            .or_else(|| {
                                self.vtables
                                    .get_index_of(&TypeVariant::complex(ComplexType::Array(ArrayType {
                                        element: TypeVariant::Generic(0).into(),
                                        size: None,
                                        array: Array {
                                            element: TypeVariant::Generic(0).as_ir_type(self.jit.module.isa()),
                                        },
                                    })))
                                    .and_then(|v| self.find_vtable_function_index_(v, function_name.as_ref()))
                            })
                            .or_else(|| {
                                self.vtables
                                    .iter()
                                    .position(|t| t.0.as_array().is_some_and(|arr| matches!(arr.element.variant, TypeVariant::Generic(_))))
                                    .and_then(|v| self.find_vtable_function_index_(v, function_name.as_ref()))
                            })
                    },
                )
            },
            |v| self.find_vtable_function_index_(v, function_name.as_ref()),
        )
    }

    pub fn get_vtable_index(&self, ty: &TypeVariant) -> Option<usize> {
        self.vtables.get_index_of(ty).map_or_else(
            || {
                ty.as_array().map_or_else(
                    || {
                        ty.as_component().map_or_else(
                            || self.vtables.get_index_of(&TypeVariant::any()),
                            |_| self.vtables.get_index_of(&TypeVariant::Primitive(PrimitiveType::Component)),
                        )
                    },
                    |ty| {
                        self.vtables
                            .get_index_of(&TypeVariant::complex(ComplexType::Array(ArrayType {
                                element: ty.element.clone(),
                                size: None,
                                array: Array {
                                    element: ty.element.variant.as_ir_type(self.jit.module.isa()),
                                },
                            })))
                            .or_else(|| {
                                self.vtables.get_index_of(&TypeVariant::complex(ComplexType::Array(ArrayType {
                                    element: TypeVariant::Generic(0).into(),
                                    size: None,
                                    array: Array {
                                        element: ty.element.variant.as_ir_type(self.jit.module.isa()),
                                    },
                                })))
                            })
                            .or_else(|| {
                                self.vtables
                                    .iter()
                                    .position(|t| t.0.as_array().is_some_and(|arr| matches!(arr.element.variant, TypeVariant::Generic(_))))
                            })
                    },
                )
            },
            Some,
        )
    }

    pub fn get_local_index<T: AsRef<str>>(&self, name: T) -> Option<usize> {
        for frame in self.frames.iter().rev() {
            if let Some(index) = frame.get_index_of(name.as_ref()) {
                return Some(index);
            }
        }

        None
    }
}

#[derive(Debug, Clone)]
pub enum ValueOrFunc {
    Value(ir::Value),
    Values(Vec<ir::Value>),
    ExtFunc(ir::SigRef, ir::Value),
    FuncRef(ir::FuncRef),
    Func(FuncId),
    Nothing,
}

pub trait Compile<T = ()> {
    /// # Errors
    ///
    /// Returns `CompileError` if compilation fails. This can happen in two
    /// cases: if the referenced variable is not found or if type inference
    /// fails.
    fn compile(self, compiler: &mut Compiler, fn_builder: &mut FunctionBuilder) -> CompileResult<T>;
}

pub trait GetType {
    /// # Errors
    ///
    /// Returns `TypeError` if type inference fails. This can happen if, for
    /// example, the required type is not declared.
    fn get_type(&self, compiler: &mut Compiler, span: Span) -> TypeResult;
}

pub trait GetPositionedType {
    /// # Errors
    ///
    /// Returns `TypeError` if type inference fails. This can happen if, for
    /// example, the required type is not declared.
    fn get_type(&self, compiler: &mut Compiler) -> TypeResult;
}

impl<T: GetType> GetPositionedType for Positioned<T> {
    fn get_type(&self, compiler: &mut Compiler) -> TypeResult {
        self.value.get_type(compiler, self.span)
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
