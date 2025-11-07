use ariadne::{Label, Report, Source};
use cranelift::{
    module::{Linkage, Module},
    prelude::{AbiParam, types},
};
use mollie_compiler::{Compiler, FuncCompiler};
use mollie_typing::{Type, TypeVariant};

fn add_builtins(func_compiler: &mut FuncCompiler) {
    func_compiler.compiler.var_ty(
        "println",
        TypeVariant::function([TypeVariant::one_of([TypeVariant::int64(), TypeVariant::usize()])], TypeVariant::void()),
    );

    func_compiler
        .compiler
        .var_ty("println_str", TypeVariant::function([TypeVariant::string()], TypeVariant::void()));

    func_compiler
        .compiler
        .var_ty("println_bool", TypeVariant::function([TypeVariant::boolean()], TypeVariant::void()));

    func_compiler
        .compiler
        .var_ty("println_float", TypeVariant::function([TypeVariant::float()], TypeVariant::void()));

    func_compiler
        .compiler
        .var_ty("println_addr", TypeVariant::function([TypeVariant::any()], TypeVariant::void()));

    func_compiler
        .compiler
        .var_ty("get_type_idx", TypeVariant::function([TypeVariant::any()], TypeVariant::usize()));

    func_compiler
        .compiler
        .var_ty("get_size", TypeVariant::function([TypeVariant::any()], TypeVariant::usize()));

    let draw_ctx_ty = TypeVariant::structure::<String, Type, _>([]);
    let draw_ctx_type_idx = func_compiler.compiler.add_type("DrawContext", draw_ctx_ty.clone());

    let color_ty = TypeVariant::structure_ir(func_compiler.compiler.jit.module.isa(), [
        ("red", TypeVariant::uint8()),
        ("green", TypeVariant::uint8()),
        ("blue", TypeVariant::uint8()),
    ]);

    func_compiler.compiler.add_type("Color", color_ty.clone());
    func_compiler.compiler.var("context", draw_ctx_type_idx);

    let draw_rect = {
        let mut sig = func_compiler.compiler.jit.module.make_signature();

        sig.params.push(AbiParam::new(func_compiler.compiler.jit.module.isa().pointer_type()));
        sig.params.push(AbiParam::new(types::F32));
        sig.params.push(AbiParam::new(types::F32));
        sig.params.push(AbiParam::new(types::F32));
        sig.params.push(AbiParam::new(types::F32));
        sig.params.push(AbiParam::new(func_compiler.compiler.jit.module.isa().pointer_type()));

        let func_id = func_compiler
            .compiler
            .jit
            .module
            .declare_function("DrawContext_draw_rect", Linkage::Import, &sig)
            .unwrap();

        let sig = func_compiler.fn_builder.import_signature(sig);
        let func = func_compiler.compiler.jit.module.declare_func_in_func(func_id, func_compiler.fn_builder.func);

        (
            TypeVariant::function_with_self(
                draw_ctx_ty,
                [TypeVariant::float(), TypeVariant::float(), TypeVariant::float(), TypeVariant::float(), color_ty],
                TypeVariant::void(),
            )
            .into(),
            (sig, func, func_id),
        )
    };

    func_compiler.create_fallback_vtable(draw_ctx_type_idx, [("draw_rect", draw_rect)]).unwrap();
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
struct Color {
    red: u8,
    green: u8,
    blue: u8,
}

#[derive(Debug)]
#[allow(dead_code)]
enum Op {
    DrawRect { x: f32, y: f32, width: f32, height: f32, color: Color },
}

struct DrawContext {
    operations: Vec<Op>,
}

impl DrawContext {
    fn draw_rect(&mut self, x: f32, y: f32, width: f32, height: f32, color: &Color) {
        println!("i was called! >:3");

        self.operations.push(Op::DrawRect {
            x,
            y,
            width,
            height,
            color: *color,
        });
    }
}

fn main() {
    let source = include_str!("../tests/drawing.mol");

    let mut compiler = Compiler::with_symbols(vec![("DrawContext_draw_rect", DrawContext::draw_rect as *const u8)]);
    let mut provider = compiler.start_compiling();
    let mut compiler = provider
        .provide_with_signature({
            let mut signature = provider.compiler.jit.module.make_signature();

            signature.params.push(AbiParam::new(provider.compiler.jit.module.isa().pointer_type()));

            signature
        })
        .unwrap();

    if let Some(block) = compiler.fn_builder.current_block() {
        let value = compiler.fn_builder.block_params(block)[0];
        let ty = compiler.fn_builder.func.signature.params[0].value_type;

        let var = compiler.fn_builder.declare_var(ty);

        compiler.compiler.variables.insert("context".to_owned(), var);
        compiler.fn_builder.def_var(var, value);
    }

    let mut draw_context = DrawContext { operations: Vec::new() };

    add_builtins(&mut compiler);

    compiler.fn_builder.func.collect_debug_info();

    let a = match compiler.compile_program_text(source) {
        Ok(main_id) => {
            provider.compiler.jit.module.define_function(main_id, &mut provider.ctx).unwrap();
            provider.compiler.jit.module.clear_context(&mut provider.ctx);
            provider.compiler.jit.module.finalize_definitions().unwrap();

            Some(unsafe { provider.compiler.get_func::<fn(*const DrawContext)>("main").unwrap() })
        }
        Err(e) => {
            match e {
                mollie_compiler::CompileError::Parse(parse_error) => Report::build(ariadne::ReportKind::Error, ("<anonymous>", 0..source.len()))
                    .with_message(parse_error.0)
                    .with_label(
                        Label::new(("<anonymous>", parse_error.1.unwrap_or_default().start..parse_error.1.unwrap_or_default().end)).with_message("here"),
                    )
                    .finish()
                    .eprint(("<anonymous>", Source::from(source)))
                    .unwrap(),
                e => println!("{e}"),
            }

            None
        }
    };

    if let Some(main_func) = a {
        main_func(&raw mut draw_context);

        println!("{:#?}", draw_context.operations);

        (unsafe { provider.compiler.get_func::<fn()>("call_me").unwrap() })();
        (unsafe { provider.compiler.get_func::<fn()>("call_me").unwrap() })();
        (unsafe { provider.compiler.get_func::<fn()>("call_me").unwrap() })();
        (unsafe { provider.compiler.get_func::<fn()>("call_me").unwrap() })();
    }
}
