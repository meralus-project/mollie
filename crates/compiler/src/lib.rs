use core::{slice, str};
use std::{fmt, sync::Arc};

use cranelift::{
    codegen::{Context, ir},
    jit::{JITBuilder, JITModule},
    module::{DataDescription, FuncId, Linkage, Module, default_libcall_names},
    native,
    prelude::{AbiParam, Configurable, FunctionBuilder, FunctionBuilderContext, InstBuilder, isa::TargetIsa, settings, types},
};
use indexmap::IndexMap;
use mollie_lexer::{Lexer, Token};
use mollie_parser::{Expr, Parser, Stmt, parse_statements_until};
use mollie_shared::{Positioned, Span};
use mollie_typing::{ArrayType, ComplexType, FatPtr, PrimitiveType, Trait, TraitFunc, Type, TypeVariant, VTablePtr};

pub use self::error::{CompileError, CompileResult, TypeError, TypeResult};

mod error;
mod statement;
mod ty;

#[derive(Debug)]
pub struct Variable {
    pub id: usize,
    pub ty: Type,
}

pub type VTable = IndexMap<Option<usize>, (ir::Value, IndexMap<String, (Type, (ir::SigRef, ir::FuncRef))>)>;

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
    pub types: IndexMap<String, Type>,
    pub impls: IndexMap<TypeVariant, Vec<usize>>,
    pub vtables: IndexMap<TypeVariant, VTable>,
    pub frames: Vec<IndexMap<String, Variable>>,

    pub generics: Vec<Type>,
    pub assign: Option<Positioned<Expr>>,
    pub this: Option<ValueOrFunc>,
    pub infer: Option<Type>,
    pub infer_ir: Option<ir::Type>,
    pub infer_val: Option<ValueOrFunc>,
    pub values: IndexMap<String, ValueOrFunc>,
    // pub variables: IndexMap<String, cranelift::prelude::Variable>,
    pub variables: IndexMap<String, cranelift::prelude::Value>,
    pub globals: IndexMap<String, FuncId>,
    pub func_names: IndexMap<FuncId, String>,

    pub jit: JitCompiler,
}

impl Default for Compiler {
    fn default() -> Self {
        Self::with_symbols(Vec::new())
    }
}

pub struct TraitBuilder<'a> {
    compiler: &'a mut Compiler,
    name: String,
    functions: Vec<TraitFunc>,
}

impl<'a> TraitBuilder<'a> {
    fn new<T: Into<String>>(compiler: &'a mut Compiler, name: T) -> Self {
        Self {
            compiler,
            name: name.into(),
            functions: Vec::new(),
        }
    }

    #[must_use]
    pub fn static_method<T: Into<String>, I: IntoIterator<Item = TypeVariant>, R: Into<Type>>(mut self, name: T, args: I, returns: R) -> Self {
        self.functions.push(TraitFunc {
            name: name.into(),
            this: false,
            args: args.into_iter().map(Into::into).collect(),
            returns: returns.into(),
            signature: ir::SigRef::from_u32(0),
        });

        self
    }

    #[must_use]
    pub fn method<T: Into<String>, I: IntoIterator<Item = TypeVariant>, R: Into<Type>>(mut self, name: T, args: I, returns: R) -> Self {
        self.functions.push(TraitFunc {
            name: name.into(),
            this: true,
            args: args.into_iter().map(Into::into).collect(),
            returns: returns.into(),
            signature: ir::SigRef::from_u32(0),
        });

        self
    }

    pub fn build(self) -> usize {
        let index = self.compiler.traits.len();

        self.compiler.traits.insert(self.name, Trait {
            generics: Vec::new(),
            functions: self.functions,
            declared_at: None,
        });

        index
    }
}

pub struct FuncCompilerBuilder<'a> {
    pub compiler: &'a mut Compiler,
    pub ctx: Context,
    pub fn_builder_ctx: FunctionBuilderContext,
}

impl FuncCompilerBuilder<'_> {
    pub fn provide(&mut self) -> FuncCompiler<'_, '_> {
        let mut ctx = &mut self.ctx;
        let mut fn_builder_ctx = &mut self.fn_builder_ctx;

        let println_id = {
            let mut do_println_sig = self.compiler.jit.module.make_signature();

            do_println_sig.params.push(AbiParam::new(types::I64));

            self.compiler.jit.module.declare_function("println", Linkage::Import, &do_println_sig).unwrap()
        };

        let println_str_id = {
            let mut do_println_sig = self.compiler.jit.module.make_signature();

            do_println_sig.params.push(AbiParam::new(types::I64));

            self.compiler
                .jit
                .module
                .declare_function("println_str", Linkage::Import, &do_println_sig)
                .unwrap()
        };

        let println_bool_id = {
            let mut do_println_sig = self.compiler.jit.module.make_signature();

            do_println_sig.params.push(AbiParam::new(types::I8));

            self.compiler
                .jit
                .module
                .declare_function("println_bool", Linkage::Import, &do_println_sig)
                .unwrap()
        };

        let println_addr_id = {
            let mut do_println_sig = self.compiler.jit.module.make_signature();

            do_println_sig.params.push(AbiParam::new(types::I64));

            self.compiler
                .jit
                .module
                .declare_function("println_addr", Linkage::Import, &do_println_sig)
                .unwrap()
        };

        let get_type_idx_id = {
            let mut get_type_idx_sig = self.compiler.jit.module.make_signature();

            get_type_idx_sig.params.push(AbiParam::new(self.compiler.jit.module.isa().pointer_type()));
            get_type_idx_sig.returns.push(AbiParam::new(self.compiler.jit.module.isa().pointer_type()));

            let func = self
                .compiler
                .jit
                .module
                .declare_function("get_type_idx", Linkage::Local, &get_type_idx_sig)
                .unwrap();

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

            self.compiler.jit.module.define_function(func, &mut ctx).unwrap();
            self.compiler.jit.module.clear_context(&mut ctx);

            func
        };

        let get_size_id = {
            let mut get_size_sig = self.compiler.jit.module.make_signature();

            get_size_sig.params.push(AbiParam::new(self.compiler.jit.module.isa().pointer_type()));
            get_size_sig.returns.push(AbiParam::new(self.compiler.jit.module.isa().pointer_type()));

            let func = self.compiler.jit.module.declare_function("get_size", Linkage::Local, &get_size_sig).unwrap();

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

            self.compiler.jit.module.define_function(func, &mut ctx).unwrap();
            self.compiler.jit.module.clear_context(&mut ctx);

            func
        };

        self.compiler.func_names.insert(println_id, "println".to_owned());
        self.compiler.func_names.insert(println_str_id, "println_str".to_owned());
        self.compiler.func_names.insert(println_bool_id, "println_bool".to_owned());
        self.compiler.func_names.insert(println_addr_id, "println_addr".to_owned());
        self.compiler.func_names.insert(get_type_idx_id, "get_type_idx".to_owned());
        self.compiler.func_names.insert(get_size_id, "get_size".to_owned());

        self.compiler.globals.insert("println".to_owned(), println_id);
        self.compiler.globals.insert("println_str".to_owned(), println_str_id);
        self.compiler.globals.insert("println_bool".to_owned(), println_bool_id);
        self.compiler.globals.insert("println_addr".to_owned(), println_addr_id);
        self.compiler.globals.insert("get_type_idx".to_owned(), get_type_idx_id);
        self.compiler.globals.insert("get_size".to_owned(), get_size_id);

        let mut fn_builder = FunctionBuilder::new(&mut ctx.func, fn_builder_ctx);

        let entry_block = fn_builder.create_block();

        fn_builder.append_block_params_for_function_params(entry_block);
        fn_builder.switch_to_block(entry_block);
        fn_builder.seal_block(entry_block);

        FuncCompiler {
            compiler: self.compiler,
            fn_builder,
        }
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

    /// # Errors
    ///
    /// Returns `CompileError` if program compilation fails.
    ///
    /// # Panics
    ///
    /// TODO
    pub fn compile_program(&mut self, (statements, returned): (Vec<Positioned<Stmt>>, Option<Positioned<Stmt>>)) -> CompileResult<FuncId> {
        for statement in statements {
            self.compiler.compile(&mut self.fn_builder, statement)?;
        }

        if let Some(statement) = returned {
            self.compiler.compile(&mut self.fn_builder, statement)?;
        }

        self.fn_builder.ins().return_(&[]);

        for func in &self.compiler.func_names {
            println!("fn{} -> {}", func.0.as_u32(), func.1);
        }

        println!("{}", self.fn_builder.func);

        Ok(self
            .compiler
            .jit
            .module
            .declare_function("main", Linkage::Export, &self.fn_builder.func.signature)
            .unwrap())
    }
}

impl Compiler {
    pub fn with_symbols(symbols: Vec<(&'static str, *const u8)>) -> Self {
        let mut flag_builder = settings::builder();

        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("opt_level", "speed").unwrap();
        flag_builder.set("is_pic", "false").unwrap();

        Self {
            traits: IndexMap::new(),
            types: IndexMap::new(),
            impls: IndexMap::new(),
            vtables: IndexMap::new(),
            frames: vec![IndexMap::new()],
            generics: Vec::new(),
            assign: None,
            this: None,
            infer: None,
            infer_ir: None,
            infer_val: None,
            jit: JitCompiler::new(symbols, settings::Flags::new(flag_builder)),
            values: IndexMap::new(),
            globals: IndexMap::new(),
            variables: IndexMap::new(),
            func_names: IndexMap::new(),
        }
    }

    pub fn get<T: AsRef<str>>(&self, name: T) -> Option<ValueOrFunc> {
        self.values.get(name.as_ref()).copied()
    }

    pub fn add_trait<T: Into<String>>(&mut self, name: T) -> TraitBuilder<'_> {
        TraitBuilder::new(self, name)
    }

    pub fn add_type<T: Into<String>>(&mut self, name: T, ty: TypeVariant) {
        self.types.insert(name.into(), ty.into());
    }

    pub fn add_declared_type<T: Into<String>>(&mut self, name: T, ty: Type) {
        self.types.insert(name.into(), ty);
    }

    pub fn remove_type<T: AsRef<str>>(&mut self, name: T) {
        self.types.shift_remove(name.as_ref());
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

    pub fn var<T: Into<String>, V: Into<Type>>(&mut self, name: T, ty: V) -> usize {
        let id = self.current_frame().len();
        let name = name.into();

        self.current_frame_mut().insert(name, Variable { id, ty: ty.into() });

        id
    }

    pub fn remove_var<T: AsRef<str>>(&mut self, name: T) {
        let name = name.as_ref();

        self.current_frame_mut().shift_remove(name);
    }

    pub fn start_compiling(&mut self) -> FuncCompilerBuilder<'_> {
        let mut ctx = self.jit.module.make_context();
        let mut fn_builder_ctx = FunctionBuilderContext::new();

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
        self.types.get(name.as_ref()).cloned().ok_or_else(|| CompileError::VariableNotFound {
            name: name.as_ref().to_string(),
        })
    }

    /// # Errors
    ///
    /// Will throw `TypeError` if there's no local with that `name`.
    pub fn get_local_type<T: AsRef<str>>(&self, name: T) -> TypeResult<Type> {
        for frame in self.frames.iter().rev() {
            if let Some(v) = frame.get(name.as_ref()) {
                return Ok(v.ty.clone());
            }
        }

        Err(TypeError::NotFound {
            ty: None,
            name: name.as_ref().to_string(),
        })
    }

    /// # Panics
    ///
    /// Will panic if there's no type with that `name`.
    pub fn get_type<T: AsRef<str>>(&self, name: T) -> Type {
        self.types
            .get(name.as_ref())
            .map_or_else(|| panic!("{} not found", name.as_ref()), Clone::clone)
    }

    fn find_vtable_function_index_<T: AsRef<str>>(&self, vtable_index: usize, function_name: T) -> Option<(usize, Option<usize>, usize)> {
        let index = self.vtables[vtable_index]
            .iter()
            .find_map(|(i, vtable)| vtable.1.get_index_of(function_name.as_ref()).map(|index| (*i, index)));

        index.map(|index| (vtable_index, index.0, index.1))
    }

    pub fn find_vtable_function<T: AsRef<str>>(&self, ty: &TypeVariant, contains: T) -> Option<&(Type, (ir::SigRef, ir::FuncRef))> {
        self.find_vtable_function_index(ty, contains).map(|v| &self.vtables[v.0][&v.1].1[v.2])
    }

    pub fn find_vtable_function_index<T: AsRef<str>>(&self, ty: &TypeVariant, function_name: T) -> Option<(usize, Option<usize>, usize)> {
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
                            })))
                            .and_then(|v| self.find_vtable_function_index_(v, function_name.as_ref()))
                            .or_else(|| {
                                self.vtables
                                    .get_index_of(&TypeVariant::complex(ComplexType::Array(ArrayType {
                                        element: TypeVariant::Generic(0).into(),
                                        size: None,
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

    pub fn get_vtable_index<T: AsRef<str>>(&self, ty: &TypeVariant) -> Option<usize> {
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
                            })))
                            .or_else(|| {
                                self.vtables.get_index_of(&TypeVariant::complex(ComplexType::Array(ArrayType {
                                    element: TypeVariant::Generic(0).into(),
                                    size: None,
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

#[derive(Debug, Clone, Copy)]
pub enum ValueOrFunc {
    Value(ir::Value),
    ExtFunc(ir::SigRef, ir::Value),
    Func(ir::FuncRef),
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

fn do_println_bool(value: i8) {
    println!("{}", value == 1);
}

fn do_println_addr(value: *mut std::ffi::c_void) {
    println!("{}", value.addr());
}

#[repr(C)]
struct MolliePtr<T> {
    ptr: *const u8,
    metadata: T,
}

fn do_println_str(value: *const MolliePtr<usize>) {
    let MolliePtr { ptr, metadata } = unsafe { value.read() };

    if let Ok(text) = str::from_utf8(unsafe { slice::from_raw_parts(ptr, metadata) }) {
        println!("{text}");
    }
}

#[cfg(test)]
mod tests {
    use cranelift::module::Module;
    use mollie_typing::TypeVariant;

    use crate::Compiler;

    fn add_builtins(compiler: &mut Compiler) {
        compiler.var(
            "println",
            TypeVariant::function(false, [TypeVariant::one_of([TypeVariant::int64(), TypeVariant::usize()])], ()),
        );
        compiler.var("println_str", TypeVariant::function(false, [TypeVariant::string()], ()));
        compiler.var("println_bool", TypeVariant::function(false, [TypeVariant::boolean()], ()));
        compiler.var("get_type_idx", TypeVariant::function(false, [TypeVariant::any()], TypeVariant::usize()));
        compiler.var("get_size", TypeVariant::function(false, [TypeVariant::any()], TypeVariant::usize()));
    }

    #[test]
    fn hmm_test() {
        let mut compiler = Compiler::default();

        add_builtins(&mut compiler);

        let mut provider = compiler.start_compiling();
        let mut compiler = provider.provide();

        let main_id = compiler
            .compile_program_text(
                r#"
        trait Placeable {
            fn place(self);
        }

        declare PlaceableComponent {
            x: int64,
            y: int64
        }

        impl trait Placeable for PlaceableComponent {
            fn place(self) {
                self.x = 4;
                
                println(11int64);
            }
        }

        declare Container {
            hello: boolean,
            children: Placeable[]
        }

        const contained = Container {
            hello: true,
            
            PlaceableComponent {
                x: 0,
                y: 0
            }
        };

        

        contained.children[0].place();

        println_str("check");

        if contained.children[0] is PlaceableComponent compik {
            println_str("accessing compik");
            println(compik.x);
            println_str("okie");
        }

        println_str("ok");
        
        const typed_num = 32uint8;
        let num = 1984;
        const str = "Hello, World!";
        const array = [4891int64, 2int64];

        num = 320;
        declare InnerComponent {
            value: int8
        }

        declare OuterComponent {
            u8value: uint8,
            u16value: uint16,
            children: component[]
        }

        struct InnerStruct {
            i64value: int64
        }

        struct OuterStruct {
            str_value: string,
            u8value: uint8,
            inner: InnerStruct
        }
        
        const comp = OuterComponent {
            u8value: 8,
            u16value: 24,
            
            InnerComponent {
                value: 4
            }
        };

        const structure = OuterStruct {
            str_value: str,
            u8value: typed_num,
            inner: InnerStruct {
                i64value: 84
            }
        };

        const placeable = PlaceableComponent {
            x: 48,
            y: 24
        };

        println_str("before");
        println(placeable.x);
        println(placeable.y);

        placeable.place();

        println_str("after");
        println(placeable.x);
        println(placeable.y);

        array[0] = 20int64;

        println(structure.inner.i64value * 84int64);
        println(get_size(str));
        println(get_size(array));
        println(array[0]);
        println(get_size(comp.children));
        println_str(str);
        
        enum Gender {
            Male,
            Female,
        }

        const my_gender = Gender::Male;

        "#,
            )
            .unwrap();

        provider.compiler.jit.module.define_function(main_id, &mut provider.ctx).unwrap();
        provider.compiler.jit.module.clear_context(&mut provider.ctx);
        provider.compiler.jit.module.finalize_definitions().unwrap();

        let code = provider.compiler.jit.module.get_finalized_function(main_id);

        unsafe { std::mem::transmute::<*const u8, fn()>(code)() };
    }
}
