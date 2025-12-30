use mollie::{
    Generic, StructBuilder, VTableBuilder,
    compiler::{
        Compiler, FuncCompiler,
        cranelift::{codegen::ir, module::Module},
    },
    typing::{ComplexTypeKind, FieldType, TypeInfo},
};

fn add_builtins(func_compiler: &mut FuncCompiler) {
    let core_types = &func_compiler.checker.core_types;

    for (name, args, returns) in [
        ("println", Box::new([core_types.int64]), core_types.void),
        ("println_str", Box::new([core_types.string]), core_types.void),
        ("println_bool", Box::new([core_types.boolean]), core_types.void),
        ("println_float", Box::new([core_types.float]), core_types.void),
        ("println_addr", Box::new([core_types.any]), core_types.void),
        ("get_type_idx", Box::new([core_types.any]), core_types.uint_size),
        ("get_size", Box::new([core_types.any]), core_types.uint_size),
    ] {
        let func = func_compiler.checker.solver.add_info(TypeInfo::Func(args, returns));

        func_compiler.checker.solver.add_var(name, func);
    }

    let color_ty = func_compiler.register_complex_type(
        StructBuilder::with_name("Color")
            .field::<u8>("red")
            .field::<u8>("green")
            .field::<u8>("blue")
            .finish(),
    );

    let plugin_ty = func_compiler.register_complex_type(StructBuilder::with_name("Plugin").add_generic().field::<Generic<0>>("data").finish());

    let plugin_info = func_compiler.instantiate_complex_type(plugin_ty, []);
    let color_info = func_compiler.instantiate_complex_type(color_ty, []);
    let draw_ctx_ty = func_compiler.register_complex_type(StructBuilder::with_name("DrawContext").finish());
    let draw_ctx_info = func_compiler.instantiate_complex_type(draw_ctx_ty, []);

    func_compiler.checker.solver.add_var("plugin", plugin_info);
    func_compiler.checker.solver.add_var("context", draw_ctx_info);

    VTableBuilder::new(FieldType::Complex(draw_ctx_ty, ComplexTypeKind::Struct, Box::new([])))
        .func(
            "draw_rect",
            "DrawContext_draw_rect",
            [
                draw_ctx_info,
                func_compiler.checker.core_types.float,
                func_compiler.checker.core_types.float,
                func_compiler.checker.core_types.float,
                func_compiler.checker.core_types.float,
                color_info,
            ],
            func_compiler.checker.core_types.void,
        )
        .finish(func_compiler.checker);
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
    let source = r#"
    struct ValueContainer {
        value: int32 = if true {
            42
        } else {
            48
        }
    }

    struct Little<T> {
        value: T
    }

    impl ValueContainer {
        fn get_value(self) -> int32 {
            self.value
        }
    }

    enum Option<T> {
        Some { value: T },
        None
    }

    const container = ValueContainer { };
    const little = Little { value: 50 };
    const little2 = Little { value: 42 };
    const little3 = Little { value: true };
    const print = |value| { println_str(value); }; // `value` infers its type based on the usage below

    let value = 5;

    while value != 10 {
        print("increasing value..");

        value = value + 1;
    }

    println_str("Hello World!");
    print("Hello World from closure!");
    
    context.draw_rect(24.0, 32.0, 96.0, 24.0, Color { red: 25, green: 78, blue: 244 });

    const hell = Option::Some { value: 2424 };

    if hell is Option::Some { value } {
        println_str("wow chat this is real?");
    }

    little.value"#;

    let mut compiler = Compiler::with_symbols(vec![("DrawContext_draw_rect", DrawContext::draw_rect as *const u8)]);
    let mut provider = compiler.start_compiling();
    let pointer_type = provider.compiler.codegen().module.isa().pointer_type();
    let mut func_compiler = provider
        .provide_with_signature(|signature| {
            signature.params.push(ir::AbiParam::new(pointer_type));
            signature.returns.push(ir::AbiParam::new(ir::types::I32));
        })
        .unwrap();

    add_builtins(&mut func_compiler);

    if let Some(block) = func_compiler.fn_builder.current_block() {
        let value = func_compiler.fn_builder.block_params(block)[0];
        let ty = func_compiler.fn_builder.func.signature.params[0].value_type;

        let var = func_compiler.fn_builder.declare_var(ty);

        func_compiler.compiler.variables.insert("context".to_owned(), var);
        func_compiler.fn_builder.def_var(var, value);
    }

    let mut draw_context = DrawContext { operations: Vec::new() };

    func_compiler.fn_builder.func.collect_debug_info();

    let a = match func_compiler.compile_program_text(source) {
        Ok(main_id) => {
            for error in std::mem::take(&mut provider.checker.solver.errors) {
                println!("{}", provider.checker.display_of_error(error));
            }

            provider.compiler.codegen_mut().module.define_function(main_id, &mut provider.ctx).unwrap();
            provider.compiler.codegen_mut().module.clear_context(&mut provider.ctx);
            provider.compiler.codegen_mut().module.finalize_definitions().unwrap();

            // Some(unsafe { provider.compiler.get_func::<fn(*const
            // DrawContext)>("<main>").unwrap() })

            let code = provider.compiler.codegen().module.get_finalized_function(main_id);

            Some(unsafe { std::mem::transmute_copy::<std::mem::ManuallyDrop<*const u8>, fn(*const DrawContext) -> i32>(&std::mem::ManuallyDrop::new(code)) })
        }
        Err(e) => {
            println!("{e}");

            None
        }
    };

    if let Some(main_func) = a {
        println!("{}", main_func(&raw mut draw_context));

        println!("{:#?}", draw_context.operations);

        // (unsafe { provider.compiler.get_func::<fn()>("call_me").unwrap()
        // })(); (unsafe { provider.compiler.get_func::<fn()>("call_me"
        // ).unwrap() })(); (unsafe { provider.compiler.
        // get_func::<fn()>("call_me").unwrap() })();
        // (unsafe { provider.compiler.get_func::<fn()>("call_me").unwrap()
        // })();
    }
}
