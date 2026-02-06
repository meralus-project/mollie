use core::fmt;
use std::{
    collections::HashMap,
    time::{Duration, Instant},
};

use mollie::{
    AdtBuilder, GcPtr, Generic, TraitBuilder, VTableBuilder,
    compiler::{Compiler, FuncCompiler, allocator::GARBAGE_COLLECTOR},
    typed_ast::Func,
    typing::{AdtKind, FieldType, FuncArg, TypeInfo, TypeInfoRef},
};
use mollie_compiler::allocator::TypeLayout;
use mollie_typing::PrimitiveType;
use tiny_skia::{FilterQuality, Paint, Pattern, Pixmap, Rect, SpreadMode, Transform};

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

#[derive(Default)]
struct ImageStorage {
    images: HashMap<String, Pixmap>,
}

impl ImageStorage {
    fn get_image(&mut self, image: GcPtr<Image>) -> &Pixmap {
        match &*image {
            Image::Path { path } => self.images.entry(path.to_string()).or_insert_with(|| Pixmap::load_png(path).unwrap()),
            Image::Url { .. } => todo!(),
        }
    }
}

pub struct DrawContext {
    root: Pixmap,
    images: ImageStorage,
    size_layout: &'static TypeLayout,
}

impl DrawContext {
    pub fn draw_rect(&mut self, x: f32, y: f32, width: f32, height: f32, color: GcPtr<Color>) {
        println!(
            "[DrawContext/draw_rect ] origin = {x}x{y}, size = {width}x{height}, color = {color} ({:?})",
            color.type_layout()
        );

        let mut paint = Paint::default();

        paint.set_color_rgba8(color.red, color.green, color.blue, 255);

        self.root
            .fill_rect(Rect::from_xywh(x, y, width, height).unwrap(), &paint, Transform::identity(), None);
    }

    pub fn draw_image(&mut self, x: f32, y: f32, width: f32, height: f32, image: GcPtr<Image>) {
        println!("[DrawContext/draw_image] origin = {x}x{y}, size = {width}x{height}, image = {image:?}");

        let image = self.images.get_image(image);
        let image_width = image.width() as f32;
        let image_height = image.height() as f32;
        let paint = Paint {
            shader: Pattern::new(
                image.as_ref(),
                SpreadMode::Pad,
                FilterQuality::Nearest,
                1.0,
                Transform::from_scale(width / image_width, height / image_height),
            ),
            ..Paint::default()
        };

        self.root
            .fill_rect(Rect::from_xywh(x, y, width, height).unwrap(), &paint, Transform::identity(), None);
    }

    pub fn image_size(&mut self, image: GcPtr<Image>) -> GcPtr<Size> {
        println!("[DrawContext/image_size] image = {image:?}");

        let image = self.images.get_image(image);
        let width = image.width() as f32;
        let height = image.height() as f32;

        GcPtr::from_parts(Size { width, height }, self.size_layout)
    }
}

pub enum Command {
    Run { name: Option<String> },
    Dump,
}

pub fn get_timestamp() -> usize {
    std::time::SystemTime::now()
        .duration_since(std::time::SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_secs() as usize
}

fn init_compiler(func_compiler: &mut FuncCompiler) -> (TypeInfoRef, TypeInfoRef) {
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

    let point_ty = func_compiler
        .checker
        .register_adt_in_module(module, AdtBuilder::new_struct("Point").field::<f32>("x").field::<f32>("y").finish());

    let (color_info, _) = func_compiler.checker.instantiate_adt(color_ty, &[]);
    let (image_info, _) = func_compiler.checker.instantiate_adt(image_ty, &[]);
    let (size_info, size_ft) = func_compiler.checker.instantiate_adt(size_ty, &[]);
    let (_, point_ft) = func_compiler.checker.instantiate_adt(point_ty, &[]);
    let draw_ctx_ty = func_compiler
        .checker
        .register_adt(AdtBuilder::new_struct("DrawContext").non_gc_collectable().finish());
    let (draw_ctx_info, draw_ctx_ft) = func_compiler.checker.instantiate_adt(draw_ctx_ty, &[]);

    let drawable = func_compiler.checker.register_trait(
        TraitBuilder::new("Drawable")
            .func("measure", [draw_ctx_ft.clone()], size_ft)
            .func("render", [point_ft, draw_ctx_ft], FieldType::Primitive(PrimitiveType::Void))
            .finish(),
    );

    let drawable_info = func_compiler.checker.solver.add_info(TypeInfo::Trait(drawable, Box::new([])));

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

    (draw_ctx_info, drawable_info)
}

fn main() {
    tracing_subscriber::fmt().init();

    let source = include_str!("./ui.mol");
    let mut args = std::env::args();

    let command = match args.nth(1).as_deref() {
        Some("dump") => Command::Dump,
        Some("run") | None => Command::Run { name: args.next() },
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

    let (draw_ctx_info, drawable_info) = init_compiler(&mut provider);

    let pointer_type = provider.compiler.ptr_type();

    let _dump = matches!(command, Command::Dump);

    provider
        .compile(
            "<main>",
            vec![(String::from("context"), pointer_type, draw_ctx_info)],
            vec![(String::from("comp"), pointer_type, drawable_info)],
            source,
        )
        .unwrap();

    for error in provider.checker.type_errors() {
        println!("[Compiler/TypeErrors   ] {error}");
    }

    match command {
        Command::Run { name } => {
            let mut draw_context = DrawContext {
                root: Pixmap::new(512, 512).unwrap(),
                images: ImageStorage::default(),
                size_layout: provider.compiler.find_adt("Size").unwrap().type_layout,
            };

            if let Some(main_func) = unsafe { provider.compiler.get_func::<fn(*const DrawContext) -> GcPtr<()>>("<main>") } {
                if name.as_deref() == Some("stress") {
                    let mut taken = Duration::ZERO;
                    let mut highest = Duration::ZERO;
                    let mut lowest = Duration::MAX;

                    for _ in 0..200_000 {
                        let instant = Instant::now();

                        main_func(&raw mut draw_context);

                        let current_taken = instant.elapsed();

                        taken += current_taken;
                        highest = highest.max(current_taken);
                        lowest = lowest.min(current_taken);
                    }

                    let mid_execution_time = taken / 200_000;
                    let gc = &*GARBAGE_COLLECTOR.lock().unwrap();

                    println!("Root objects: {}", gc.roots.len());
                    println!("Allocated objects: {}", gc.objects.len());
                    println!("Bytes allocated: {}", gc.allocated_bytes);
                    println!("Bytes dealloacted: {}", gc.deallocated_bytes);

                    println!("Execution time (sum): {taken:?} for 200k calls");
                    println!("Execution time (avg): ~{mid_execution_time:?}");
                    println!("Execution time (max): ~{highest:?}");
                    println!("Execution time (min): ~{lowest:?}");

                    println!("Calls per sec: ~{:.2}", 1.0 / mid_execution_time.as_secs_f32());
                } else {
                    let value = main_func(&raw mut draw_context);
                    let ty = value.type_layout();

                    if let Some(hash) = ty.adt_ty {
                        #[derive(Debug)]
                        #[repr(C)]
                        struct Point {
                            x: f32,
                            y: f32,
                        }

                        #[derive(Debug, Clone, Copy)]
                        #[repr(C)]
                        struct Drawable {
                            measure: fn(*mut (), &mut DrawContext) -> GcPtr<Size>,
                            render: fn(*mut (), point: GcPtr<Point>, ctx: &mut DrawContext),
                        }

                        let trait_ref = provider.checker.find_trait("Drawable");

                        if let Some(vtable) = unsafe { compiler.get_vtable_ptr::<Drawable>(hash, trait_ref) } {
                            (vtable.render)(value.ptr_mut(), GcPtr::from(Point { x: 0.0, y: 0.0 }), &mut draw_context);
                        }
                    }

                    draw_context.root.save_png("./output.png").unwrap();
                }
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
