use core::fmt;
use std::{
    collections::HashMap,
    path::PathBuf,
    time::{Duration, Instant},
};

use ariadne::{Config, Report, ReportKind, Source};
use mollie::{
    AdtBuilder, AnyType, GcPtr, Generic, TraitBuilder, VTableBuilder,
    compiler::{Compiler, allocator::GARBAGE_COLLECTOR},
    func,
};
use mollie_compiler::{allocator::TypeLayout, error::CompileError};
use mollie_index::Idx;
use mollie_typed_ast::{FileModuleLoader, FunctionBody, TypedASTContext};
use mollie_typing::{Func, ModuleId, Type, TypeRef};
use tiny_skia::{FillRule, FilterQuality, Paint, PathBuilder, Pattern, Pixmap, Point, Rect, SpreadMode, Transform};
use tracing::{Level, level_filters::LevelFilter};
use tracing_subscriber::{Layer, filter::filter_fn, layer::SubscriberExt, util::SubscriberInitExt};

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

#[repr(C)]
pub struct CornerRadius {
    pub top_left: f32,
    pub top_right: f32,
    pub bottom_left: f32,
    pub bottom_right: f32,
}

struct Path {
    builder: PathBuilder,
}

impl Path {
    fn new() -> Self {
        Self { builder: PathBuilder::new() }
    }

    fn move_to(&mut self, point: Point) {
        self.builder.move_to(point.x, point.y);
    }

    fn line_to(&mut self, point: Point) {
        self.builder.line_to(point.x, point.y);
    }

    fn quad_to(&mut self, point1: Point, point: Point) {
        self.builder.quad_to(point1.x, point1.y, point.x, point.y);
    }

    fn close(&mut self) {
        self.builder.close();
    }

    fn finish(self) -> Option<tiny_skia::Path> {
        self.builder.finish()
    }
}

impl DrawContext {
    pub fn draw_rect(&mut self, x: f32, y: f32, width: f32, height: f32, corner_radius: GcPtr<CornerRadius>, color: GcPtr<Color>) {
        tracing::info!(target: "DrawContext/draw_rect", origin = format!("{x}x{y}"), size = format!("{width}x{height}"), color = %color);

        let mut paint = Paint::default();

        paint.set_color_rgba8(color.red, color.green, color.blue, 255);

        let mut path = Path::new();

        path.move_to(Point::from_xy(x + corner_radius.top_left, y));
        path.line_to(Point::from_xy(x + width - corner_radius.top_right, y));
        path.quad_to(Point::from_xy(x + width, y), Point::from_xy(x + width, y + corner_radius.top_right));
        path.line_to(Point::from_xy(x + width, y + height - corner_radius.bottom_right));
        path.quad_to(
            Point::from_xy(x + width, y + height),
            Point::from_xy(x + width - corner_radius.bottom_right, y + height),
        );
        path.line_to(Point::from_xy(x + corner_radius.bottom_left, y + height));
        path.quad_to(Point::from_xy(x, y + height), Point::from_xy(x, y + height - corner_radius.bottom_left));
        path.line_to(Point::from_xy(x, y + corner_radius.top_left));
        path.quad_to(Point::from_xy(x, y), Point::from_xy(x + corner_radius.top_left, y));
        path.close();

        self.root
            .fill_path(&path.finish().unwrap(), &paint, FillRule::Winding, Transform::identity(), None);
    }

    pub fn draw_image(&mut self, x: f32, y: f32, width: f32, height: f32, image: GcPtr<Image>) {
        tracing::info!(target: "DrawContext/draw_image", origin = format!("{x}x{y}"), size = format!("{width}x{height}"), image = format!("{image:?}"));

        let image = self.images.get_image(image);
        let image_width = image.width() as f32;
        let image_height = image.height() as f32;
        let paint = Paint {
            shader: Pattern::new(
                image.as_ref(),
                SpreadMode::Pad,
                FilterQuality::Nearest,
                1.0,
                Transform::from_scale(width / image_width, height / image_height).post_translate(x, y),
            ),
            ..Paint::default()
        };

        self.root
            .fill_rect(Rect::from_xywh(x, y, width, height).unwrap(), &paint, Transform::identity(), None);
    }

    pub fn image_size(&mut self, image: GcPtr<Image>) -> GcPtr<Size> {
        tracing::info!(target: "DrawContext/image_size", image = format!("{image:?}"));

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

fn init_compiler(context: &mut TypedASTContext) -> (TypeRef, TypeRef) {
    // let std = mollie::module! {
    //     fn println(input: usize);
    //     fn println_frame_addr();
    //     fn println_fat(input: &str);
    //     fn println_str(input: &str);
    //     fn println_bool(input: bool);
    //     fn println_float(input: f32);
    //     fn println_addr(input: AnyType);
    //     fn get_type_idx(input: AnyType) -> usize;
    //     fn get_size(input: &[AnyType]) -> usize;

    //     #[from = "ext__get_timestamp"]
    //     fn timestamp() -> usize;
    // };

    let module = context.type_context.register_module("std");

    for (module, external_name, name, ty) in [
        (None, None, "println", func::<(usize,), ()>(&mut context.type_context)),
        (None, None, "println_frame_addr", func::<(), ()>(&mut context.type_context)),
        (None, None, "println_fat", func::<(&str,), ()>(&mut context.type_context)),
        (None, None, "println_str", func::<(&str,), ()>(&mut context.type_context)),
        (None, None, "println_bool", func::<(bool,), ()>(&mut context.type_context)),
        (None, None, "println_f32", func::<(f32,), ()>(&mut context.type_context)),
        (None, None, "println_addr", func::<(AnyType,), ()>(&mut context.type_context)),
        (None, None, "get_type_idx", func::<(AnyType,), usize>(&mut context.type_context)),
        (None, None, "get_size", func::<(&[AnyType],), usize>(&mut context.type_context)),
        (
            Some(module),
            Some("ext__get_timestamp"),
            "timestamp",
            func::<(), usize>(&mut context.type_context),
        ),
    ] {
        context.functions.push(FunctionBody::Import(external_name.unwrap_or(name)));
        context.type_context.register_func_in_module(module.unwrap_or(ModuleId::ZERO), Func {
            postfix: false,
            name: name.to_string(),
            arg_names: Vec::new(),
            ty,
        });
    }

    // let func_arg = context.type_context.solver.add_info(TypeInfo::Generic(0,
    // None), None);
    // let ty = TypeInfo::Func(Box::new([FuncArg::Regular(func_arg)]),
    // context.type_context.core_types.uint_size);

    // context
    //     .type_context
    //     .register_intrinsic_in_module(module, "size_of_val",
    // IntrinsicKind::SizeOfValue, ty);

    AdtBuilder::new_enum(&mut context.type_context, "Option")
        .add_generic()
        .variant("Some")
        .field::<Generic<0>>("value")
        .variant("None")
        .finish_in_module(module);

    let module = context.type_context.register_module("graphics");
    let image_ty = AdtBuilder::new_enum(&mut context.type_context, "Image")
        .variant("Path")
        .field::<&str>("value")
        .variant("Url")
        .field::<&str>("value")
        .finish_in_module(module);
    let image_ty = context.type_context.types.get_or_add(Type::Adt(image_ty, Box::new([])));

    let color_ty = AdtBuilder::new_struct(&mut context.type_context, "Color")
        .field::<u8>("red")
        .field::<u8>("green")
        .field::<u8>("blue")
        .finish_in_module(module);
    let color_ty = context.type_context.types.get_or_add(Type::Adt(color_ty, Box::new([])));

    let corner_radius_ty = AdtBuilder::new_struct(&mut context.type_context, "CornerRadius")
        .field::<f32>("top_left")
        .field::<f32>("top_right")
        .field::<f32>("bottom_left")
        .field::<f32>("bottom_right")
        .finish_in_module(module);
    let corner_radius_ty = context.type_context.types.get_or_add(Type::Adt(corner_radius_ty, Box::new([])));

    let size_ty = AdtBuilder::new_struct(&mut context.type_context, "Size")
        .field::<f32>("width")
        .field::<f32>("height")
        .finish_in_module(module);
    let size_ty = context.type_context.types.get_or_add(Type::Adt(size_ty, Box::new([])));

    let point_ty = AdtBuilder::new_struct(&mut context.type_context, "Point")
        .field::<f32>("x")
        .field::<f32>("y")
        .finish_in_module(module);
    let point_ty = context.type_context.types.get_or_add(Type::Adt(point_ty, Box::new([])));

    let draw_ctx_ty = AdtBuilder::new_struct(&mut context.type_context, "DrawContext").non_gc_collectable().finish();
    let draw_ctx_ty = context.type_context.types.get_or_add(Type::Adt(draw_ctx_ty, Box::new([])));

    let void = context.type_context.core_types.void;
    let drawable = TraitBuilder::new(&mut context.type_context, "Drawable")
        .func("measure", [("size", size_ty), ("draw_context", draw_ctx_ty)], size_ty)
        .func("render", [("origin", point_ty), ("size", size_ty), ("draw_context", draw_ctx_ty)], void)
        .finish();
    let drawable = context.type_context.types.get_or_add(Type::Trait(drawable, Box::new([])));

    let f32 = context.type_context.core_types.f32;

    VTableBuilder::new(context, draw_ctx_ty)
        .func(
            "draw_rect",
            "DrawContext_draw_rect",
            [draw_ctx_ty, f32, f32, f32, f32, corner_radius_ty, color_ty],
            void,
        )
        .func("draw_image", "DrawContext_draw_image", [draw_ctx_ty, f32, f32, f32, f32, image_ty], void)
        .func("image_size", "DrawContext_image_size", [draw_ctx_ty, image_ty], size_ty)
        .finish();

    (draw_ctx_ty, drawable)
}

fn main() {
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::fmt::layer()
                .with_filter(LevelFilter::INFO)
                .with_filter(filter_fn(|metadata| {
                    !(metadata.target() == "cranelift_jit::backend" && metadata.level() == &Level::INFO)
                })),
        )
        .init();

    let source = include_str!("./ui.mol");
    let mut args = std::env::args();

    let command = match args.nth(1).as_deref() {
        Some("dump") => Command::Dump,
        Some("run") | None => Command::Run { name: args.next() },
        Some(command) => panic!("unknown command: {command}"),
    };

    let mut compiler = Compiler::with_symbols(
        FileModuleLoader {
            current_dir: PathBuf::from("/home/aiving/Documents/dev-v2/dev/meralus-project/mollie/examples"),
        },
        [
            ("DrawContext_draw_rect", DrawContext::draw_rect as *const u8),
            ("DrawContext_draw_image", DrawContext::draw_image as *const u8),
            ("DrawContext_image_size", DrawContext::image_size as *const u8),
            ("ext__get_timestamp", get_timestamp as *const u8),
        ],
    )
    .unwrap();

    let mut provider = compiler.start_compiling();

    let (draw_ctx_info, drawable_info) = init_compiler(provider.type_context);

    let pointer_type = provider.compiler.ptr_type();

    let _dump = matches!(command, Command::Dump);

    if let Err(CompileError::Type(errors)) = provider.compile(
        "<main>",
        [("context", pointer_type, draw_ctx_info)],
        Some((pointer_type, drawable_info)),
        source,
    ) {
        for error in errors {
            let mut report = Report::build(ReportKind::Error, ("ui.mol", error.span.start..error.span.end)).with_config(Config::new().with_compact(true));

            error
                .value
                .add_to_report(("ui.mol", error.span.start..error.span.end), &mut report, &provider.type_context.type_context);

            report.finish().print(("ui.mol", Source::from(source))).unwrap();
        }

        return;
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
                    let mut value = main_func(&raw mut draw_context);
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
                            measure: fn(*mut (), parent: GcPtr<Size>, &mut DrawContext) -> GcPtr<Size>,
                            render: fn(*mut (), point: GcPtr<Point>, parent: GcPtr<Size>, ctx: &mut DrawContext),
                        }

                        let trait_ref = provider.type_context.type_context.find_trait("Drawable");

                        if let Some(vtable) = unsafe { provider.compiler.get_vtable_ptr::<Drawable>(hash, trait_ref) } {
                            (vtable.render)(
                                value.ptr_mut(),
                                GcPtr::from(Point { x: 0.0, y: 0.0 }),
                                GcPtr::from(Size { width: 512.0, height: 512.0 }),
                                &mut draw_context,
                            );
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
