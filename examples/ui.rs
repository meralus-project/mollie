use core::fmt;

use mollie::{
    AdtBuilder, GcPtr, Generic, VTableBuilder, compiler::{Compiler, FuncCompiler}, typed_ast::Func, typing::{AdtKind, FieldType, FuncArg, TypeInfo}
};
use mollie_compiler::allocator::GARBAGE_COLLECTOR;
use mollie_typing::TypeInfoRef;

#[derive(Debug, Clone, Copy)]
pub struct Color {
    pub red: u8,
    pub green: u8,
    pub blue: u8,
}

impl fmt::Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "rgb({}, {}, {})", self.red, self.green, self.blue)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Size {
    pub width: f32,
    pub height: f32,
}

#[derive(Debug, Clone, Copy)]
#[repr(usize)]
pub enum Image<'a> {
    Path { path: &'a str },
    Url { url: &'a str },
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum Action {
    DrawRect { x: f32, y: f32, width: f32, height: f32, color: Color },
    DrawImage { x: f32, y: f32, width: f32, height: f32, image: String },
}

pub struct DrawContext {
    actions: Vec<Action>,
}

impl DrawContext {
    pub fn draw_rect(&mut self, x: f32, y: f32, width: f32, height: f32, color: GcPtr<Color>) {
        println!("[DrawContext/draw_rect ] origin = {x}x{y}, size = {width}x{height}, color = {color} ({:?})", color.type_layout());

        self.actions.push(Action::DrawRect {
            x,
            y,
            width,
            height,
            color: *color,
        });
    }

    pub fn draw_image(&mut self, x: f32, y: f32, width: f32, height: f32, image: &Image) {
        println!("[DrawContext/draw_image] origin = {x}x{y}, size = {width}x{height}, image = {image:?}");

        // self.actions.push(Action::DrawImage {
        //     x,
        //     y,
        //     width,
        //     height,
        //     image: (value.to_string(), image),
        // });
    }

    pub fn image_size(&self, image: &Image) -> *mut Size {
        println!("[DrawContext/image_size] image = {image:?}");

        Box::into_raw(Box::new(Size { width: 24.0, height: 24.0 }))
    }
}

pub enum Command {
    Run,
    Dump,
}

pub fn get_timestamp() -> usize {
    std::time::SystemTime::now()
        .duration_since(std::time::SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_secs() as usize
}

fn init_compiler(func_compiler: &mut FuncCompiler) -> TypeInfoRef {
    let core_types = &func_compiler.checker.core_types;

    for (name, args, returns) in [
        (
            "println",
            Box::new([FuncArg::Regular(core_types.uint_size)]) as Box<[FuncArg<TypeInfoRef>]>,
            core_types.void,
        ),
        ("println_frame_addr", Box::new([]), core_types.void),
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

    let module = func_compiler.checker.register_module("std");

    func_compiler
        .checker
        .register_func_in_module(module, Func::external("timestamp", func, "ext__get_timestamp"));
    func_compiler.checker.register_adt_in_module(
        module,
        AdtBuilder::new_enum("Option")
            .add_generic()
            .variant("Some")
            .field::<Generic<0>>("value")
            .variant("None")
            .finish(),
    );

    let module = func_compiler.checker.register_module("graphics");
    let image_ty = func_compiler.checker.register_adt_in_module(
        module,
        AdtBuilder::new_enum("Image")
            .variant("Path")
            .field::<&str>("value")
            .variant("Url")
            .field::<&str>("value")
            .finish(),
    );

    let color_ty = func_compiler.checker.register_adt_in_module(
        module,
        AdtBuilder::new_struct("Color")
            .field::<u8>("red")
            .field::<u8>("green")
            .field::<u8>("blue")
            .finish(),
    );

    let size_ty = func_compiler
        .checker
        .register_adt_in_module(module, AdtBuilder::new_struct("Size").field::<f32>("width").field::<f32>("height").finish());

    let color_info = func_compiler.checker.instantiate_adt(color_ty, &[]);
    let image_info = func_compiler.checker.instantiate_adt(image_ty, &[]);
    let size_info = func_compiler.checker.instantiate_adt(size_ty, &[]);
    let draw_ctx_ty = func_compiler.checker.register_adt(AdtBuilder::new_struct("DrawContext").finish());
    let draw_ctx_info = func_compiler.checker.instantiate_adt(draw_ctx_ty, &[]);

    func_compiler.checker.solver.add_var("context", draw_ctx_info);

    VTableBuilder::new(FieldType::Adt(draw_ctx_ty, AdtKind::Struct, Box::new([])))
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

    draw_ctx_info
}

fn main() {
    let source = include_str!("./ui.mol");

    let command = match std::env::args().nth(1).as_deref() {
        Some("dump") => Command::Dump,
        Some("run") | None => Command::Run,
        Some(command) => panic!("unknown command: {command}"),
    };

    let mut compiler = Compiler::with_symbols(vec![
        ("DrawContext_draw_rect", DrawContext::draw_rect as *const u8),
        ("DrawContext_draw_image", DrawContext::draw_image as *const u8),
        ("DrawContext_image_size", DrawContext::image_size as *const u8),
        ("ext__get_timestamp", get_timestamp as *const u8),
    ])
    .unwrap();
    let mut provider = compiler.start_compiling();

    let draw_ctx_info = init_compiler(&mut provider);

    let pointer_type = provider.compiler.ptr_type();
    let mut draw_context = DrawContext { actions: Vec::new() };

    let _dump = matches!(command, Command::Dump);

    provider
        .compile("<main>", vec![(String::from("context"), pointer_type, draw_ctx_info)], source)
        .unwrap();

    for error in provider.checker.type_errors() {
        println!("[Compiler/TypeErrors   ] {error}");
    }

    match command {
        Command::Run => {
            if let Some(main_func) = unsafe { provider.compiler.get_func::<fn(*const DrawContext)>("<main>") } {
                for _ in 0..1 {
                    main_func(&raw mut draw_context);
                }

                let allocator = GARBAGE_COLLECTOR.lock().unwrap();

                println!(
                    "Allocated objects: {} ({} bytes allocated, {} bytes dealloacted)",
                    allocator.objects.len(),
                    allocator.allocated_bytes,
                    allocator.deallocated_bytes
                );
                println!("Root objects: {}", allocator.roots.len());
            }
        }
        Command::Dump => {
            println!(">> Dumping functions");

            // let longest_name =
            // provider.compiler.func_names.values().map(|name|
            // name.len()).max().unwrap_or(1); let longest_func_id =
            // provider.compiler.func_names.keys().map(|id|
            // id.to_string().len()).max().unwrap_or(7);
            //
            // for (func_id, name) in &provider.compiler.func_names {
            //    let length = if longest_name >= name.len() { longest_name -
            // name.len() + 1 } else { 1 };    let func_length =
            // longest_func_id - func_id.to_string().len() + 1;
            //
            //    println!(
            //        "Function `{name}`{:length$}signature:
            // {func_id}{:func_length$}{}",        " ",
            //        " ",
            //        provider.compiler.codegen().module.declarations().
            // get_function_decl(*func_id).signature,    )
            //}
        }
    }
}
