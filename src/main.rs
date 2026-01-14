use std::alloc::{Layout, alloc};

use mollie::{
    Generic, StructBuilder, VTableBuilder,
    compiler::{Compiler, cranelift::module::Module},
    typing::{ComplexTypeKind, FieldType, TypeInfo},
};
use mollie_compiler::{FuncCompiler, cranelift::codegen::write::decorate_function};
use mollie_tast::{Func, VTableFuncKind};
use mollie_typing::FuncArg;

fn add_builtins(func_compiler: &mut FuncCompiler) {
    let core_types = &func_compiler.checker.core_types;

    for (name, args, returns) in [
        ("println", Box::new([FuncArg::Regular(core_types.int64)]), core_types.void),
        ("println_fat", Box::new([FuncArg::Regular(core_types.string)]), core_types.void),
        ("println_str", Box::new([FuncArg::Regular(core_types.string)]), core_types.void),
        ("println_bool", Box::new([FuncArg::Regular(core_types.boolean)]), core_types.void),
        ("println_float", Box::new([FuncArg::Regular(core_types.float)]), core_types.void),
        ("println_addr", Box::new([FuncArg::Regular(core_types.any)]), core_types.void),
        ("get_type_idx", Box::new([FuncArg::Regular(core_types.any)]), core_types.uint_size),
        ("get_size", Box::new([FuncArg::Regular(core_types.any)]), core_types.uint_size),
    ] {
        let func = func_compiler.checker.solver.add_info(TypeInfo::Func(args, returns));

        func_compiler.checker.solver.add_var(name, func);
    }

    let func = func_compiler.checker.solver.add_info(TypeInfo::Func(Box::new([]), core_types.uint_size));

    let module = func_compiler.register_module("std");

    func_compiler.register_func_in_module(module, Func {
        name: String::from("timestamp"),
        arg_names: vec![],
        ty: func,
        kind: VTableFuncKind::External("ext__get_timestamp"),
    });

    func_compiler.register_complex_type_in_module(
        module,
        StructBuilder::enum_with_name("Option")
            .add_generic()
            .variant("Some")
            .field::<Generic<0>>("value")
            .variant("None")
            .finish(),
    );

    let module = func_compiler.register_module("graphics");

    let image_ty = func_compiler.register_complex_type_in_module(
        module,
        StructBuilder::enum_with_name("Image")
            .variant("Path")
            .field::<&str>("value")
            .variant("Url")
            .field::<&str>("value")
            .finish(),
    );

    let color_ty = func_compiler.register_complex_type_in_module(
        module,
        StructBuilder::with_name("Color")
            .field::<u8>("red")
            .field::<u8>("green")
            .field::<u8>("blue")
            .finish(),
    );

    let size_ty = func_compiler.register_complex_type_in_module(module, StructBuilder::with_name("Size").field::<f32>("width").field::<f32>("height").finish());

    // let plugin_ty =
    // func_compiler.register_complex_type(StructBuilder::with_name("Plugin").
    // add_generic().field::<Generic<0>>("data").finish());

    // let plugin_info = func_compiler.instantiate_complex_type(plugin_ty, []);
    let color_info = func_compiler.instantiate_complex_type(color_ty, []);
    let image_info = func_compiler.instantiate_complex_type(image_ty, []);
    let size_info = func_compiler.instantiate_complex_type(size_ty, []);
    let draw_ctx_ty = func_compiler.register_complex_type(StructBuilder::with_name("DrawContext").finish());
    let draw_ctx_info = func_compiler.instantiate_complex_type(draw_ctx_ty, []);

    // func_compiler.checker.solver.add_var("plugin", plugin_info);
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
        .func(
            "draw_image",
            "DrawContext_draw_image",
            [
                draw_ctx_info,
                func_compiler.checker.core_types.float,
                func_compiler.checker.core_types.float,
                func_compiler.checker.core_types.float,
                func_compiler.checker.core_types.float,
                image_info,
            ],
            func_compiler.checker.core_types.void,
        )
        .func("image_size", "DrawContext_image_size", [draw_ctx_info, image_info], size_info)
        .finish(&mut func_compiler.checker);
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
struct Color {
    red: u8,
    green: u8,
    blue: u8,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
enum ImageKind {
    Path,
    Url,
}

#[derive(Debug, Clone, Copy)]
enum Image<'a> {
    Path(&'a str),
    Url(&'a str),
}

#[derive(Debug)]
#[allow(dead_code)]
enum Op {
    DrawRect {
        x: f32,
        y: f32,
        width: f32,
        height: f32,
        color: Color,
    },
    DrawImage {
        x: f32,
        y: f32,
        width: f32,
        height: f32,
        image: (String, ImageKind),
    },
}

struct Size {
    width: f32,
    height: f32,
}

struct DrawContext {
    operations: Vec<Op>,
}

impl DrawContext {
    fn draw_rect(&mut self, x: f32, y: f32, width: f32, height: f32, color: &Color) {
        println!("[draw_rect] i was called! >:3");

        self.operations.push(Op::DrawRect {
            x,
            y,
            width,
            height,
            color: *color,
        });
    }

    fn draw_image(&mut self, x: f32, y: f32, width: f32, height: f32, (value, image): (&&str, usize)) {
        println!("[draw_image] i was called! >:3");

        self.operations.push(Op::DrawImage {
            x,
            y,
            width,
            height,
            image: (value.to_string(), if image == 0 { ImageKind::Path } else { ImageKind::Url }),
        });
    }

    fn image_size(&self, (value, image): (&&str, usize)) -> *mut Size {
        println!("[image_size] i was called! >:3");

        let size = unsafe { alloc(Layout::new::<Size>()) };
        let size = size.cast();

        unsafe { *size = Size { width: 24.0, height: 24.0 } };

        size
    }
}

enum Command {
    Run,
    Dump,
}

fn get_timestamp() -> usize {
    std::time::SystemTime::now()
        .duration_since(std::time::SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_secs() as usize
}

fn main() {
    let source = r#"
    import { Option, timestamp } from std;
    import { Size } from graphics;

    trait Iterable<T> {
        fn iter(self) -> T;
    }

    trait Iterator<T> {
        fn next(self) -> Option<T>;
    }

    struct ArrayIter<T> {
        value: T[],
        index: uint_size
    }

    impl<T> Iterator<T> for ArrayIter<T> {
        fn next(self) -> Option<T> {
            if self.index == get_size(self.value) {
                Option::None
            } else {
                const value = self.value[self.index];

                self.index = self.index + 1;

                Option::Some { value }
            }
        }
    }

    impl<T> Iterable<ArrayIter<T>> for T[] {
        fn iter(self) -> ArrayIter<T> {
            ArrayIter {
                value: self,
                index: 0
            }
        }
    }

    trait Drawable {
        fn measure(self, ctx: DrawContext) -> Size;
        fn render(self, ctx: DrawContext);
    }

    declare Rectangle {
        width: float,
        height: float,
        background: graphics::Color
    }

    declare Image {
        width: float = 0.0,
        height: float = 0.0,
        image: graphics::Image
    }

    impl graphics::Color {
        fn new(red: uint8, green: uint8, blue: uint8) -> graphics::Color {
            graphics::Color { red, green, blue }
        }
    }

    declare Column {
        children: Drawable[]
    }

    impl Drawable for Image {
        fn measure(self, ctx: DrawContext) -> Size {
            const width = if self.width > 0.0 {
                self.width
            } else {
                ctx.image_size(self.image).width
            };

            const height = if self.height > 0.0 {
                self.height
            } else {
                ctx.image_size(self.image).height
            };

            self.width = width;
            self.height = height;

            Size { width, height }
        }

        fn render(self, ctx: DrawContext) {
            ctx.draw_image(24.0, 32.0, self.width, self.height, self.image);
        }
    }

    impl Drawable for Rectangle {
        fn measure(self, ctx: DrawContext) -> Size {
            Size {
                width: self.width,
                height: self.height
            }
        }

        fn render(self, ctx: DrawContext) {
            ctx.draw_rect(24.0, 32.0, self.width, self.height, self.background);
        }
    }

    impl float {
        fn max(self, other: float) -> float {
            if self > other {
                self
            } else {
                other
            }
        }
    }

    impl Drawable for Column {
        fn measure(self, ctx: DrawContext) -> Size {
            const width = 0.0;
            const height = 0.0;

            const iterator = self.children.iter();

            while iterator.next() is Option::Some { value } {
                const measured = value.measure(ctx);

                width = measured.width.max(width);
                height = height + measured.height;
            }

            Size { width, height }
        }

        fn render(self, ctx: DrawContext) {
            const iterator = self.children.iter();

            while iterator.next() is Option::Some { value } {
                value.render(ctx);
            }
        }
    }

    impl graphics::Image {
        fn path(value: string) -> graphics::Image {
            graphics::Image::Path { value }
        }
    }

    const width = 24.0;

    const column = Column {
        Image {
            image: graphics::Image::path("frustrating")
        }

        Rectangle {
            width,
            height: 48.0,
            background: graphics::Color::new(255, 0, 0),
        }

        Rectangle {
            width: 96.0,
            height: 48.0,
            background: graphics::Color::new(0, 0, 255),
        }
    };

    const column_size = column.measure(context);

    println_float(column_size.width);
    println_float(column_size.height);

    println(timestamp());

    column.render(context);"#;

    let command = match std::env::args().nth(1).as_deref() {
        Some("dump") => Command::Dump,
        Some("run") | None => Command::Run,
        Some(command) => panic!("unknown command: {command}"),
    };

    // println!("Input args: [{}]", .join(", "));

    let mut compiler = Compiler::with_symbols(vec![
        ("DrawContext_draw_rect", DrawContext::draw_rect as *const u8),
        ("DrawContext_draw_image", DrawContext::draw_image as *const u8),
        ("DrawContext_image_size", DrawContext::image_size as *const u8),
        ("ext__get_timestamp", get_timestamp as *const u8),
    ])
    .unwrap();
    let mut provider = compiler.start_compiling();

    add_builtins(&mut provider);

    let pointer_type = provider.compiler.codegen().module.isa().pointer_type();
    let mut draw_context = DrawContext { operations: Vec::new() };

    let dump = matches!(command, Command::Dump);

    provider.compile("<main>", vec![(String::from("context"), pointer_type)], source, dump).unwrap();

    for error in provider.type_errors() {
        println!("{error}");
    }

    match command {
        Command::Run => {
            if let Some(main_func) = unsafe { provider.compiler.get_func::<fn(*const DrawContext)>("<main>") } {
                main_func(&raw mut draw_context);
            }

            for op in &draw_context.operations {
                println!("{op:?}");
            }
        }
        Command::Dump => {
            println!(">> Dumping functions");

            for (func_id, name) in &provider.compiler.func_names {
                println!(
                    "Function `{name}` signature: {func_id} {}",
                    provider.compiler.codegen().module.declarations().get_function_decl(*func_id).signature
                )
            }
        }
    }
}

struct Little<T> {
    value: T,
}
