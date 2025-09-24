use ariadne::{Label, Report, Source};
use cranelift::{
    module::{Linkage, Module},
    prelude::{AbiParam, InstBuilder, types},
};
use indexmap::IndexMap;
use mollie_compiler::{Compiler, FuncCompiler, VTable};
use mollie_shared::cranelift::stack_alloc;
use mollie_typing::{Type, TypeVariant};

fn add_builtins(func_compiler: &mut FuncCompiler, context: &mut DrawContext) {
    func_compiler.compiler.var(
        "println",
        TypeVariant::function(false, [TypeVariant::one_of([TypeVariant::int64(), TypeVariant::usize()])], ()),
    );

    func_compiler
        .compiler
        .var("println_str", TypeVariant::function(false, [TypeVariant::string()], ()));
    func_compiler
        .compiler
        .var("println_bool", TypeVariant::function(false, [TypeVariant::boolean()], ()));
    func_compiler
        .compiler
        .var("println_addr", TypeVariant::function(false, [TypeVariant::any()], ()));
    func_compiler
        .compiler
        .var("get_type_idx", TypeVariant::function(false, [TypeVariant::any()], TypeVariant::usize()));
    func_compiler
        .compiler
        .var("get_size", TypeVariant::function(false, [TypeVariant::any()], TypeVariant::usize()));

    let draw_ctx_ty = TypeVariant::structure::<String, Type, _>([]);

    func_compiler.compiler.add_type("DrawContext", draw_ctx_ty.clone());

    let var = func_compiler
        .fn_builder
        .declare_var(draw_ctx_ty.as_ir_type(func_compiler.compiler.jit.module.isa()));

    func_compiler.compiler.var("context", draw_ctx_ty.clone());

    let value = func_compiler.fn_builder.ins().iconst(
        func_compiler.compiler.jit.module.isa().pointer_type(),
        std::ptr::from_mut::<DrawContext>(context).addr().cast_signed() as i64,
    );

    func_compiler.compiler.variables.insert("context".to_string(), var);
    func_compiler.fn_builder.def_var(var, value);

    let draw_rect = {
        let mut sig = func_compiler.compiler.jit.module.make_signature();

        sig.params.push(AbiParam::new(func_compiler.compiler.jit.module.isa().pointer_type()));
        sig.params.push(AbiParam::new(types::F32));
        sig.params.push(AbiParam::new(types::F32));
        sig.params.push(AbiParam::new(types::F32));
        sig.params.push(AbiParam::new(types::F32));
        sig.params.push(AbiParam::new(types::I32));

        let func_id = func_compiler
            .compiler
            .jit
            .module
            .declare_function("DrawContext_draw_rect", Linkage::Import, &sig)
            .unwrap();
        let sig = func_compiler.fn_builder.import_signature(sig);
        let func = func_compiler.compiler.jit.module.declare_func_in_func(func_id, func_compiler.fn_builder.func);

        (
            TypeVariant::function(
                true,
                [
                    draw_ctx_ty.clone(),
                    TypeVariant::float(),
                    TypeVariant::float(),
                    TypeVariant::float(),
                    TypeVariant::float(),
                    TypeVariant::uint32(),
                ],
                (),
            )
            .into(),
            (sig, func, func_id),
        )
    };

    let functions = IndexMap::from_iter([(String::from("draw_rect"), draw_rect)]);

    let size_t = func_compiler.compiler.jit.module.isa().pointer_type();
    let slot = stack_alloc(&mut func_compiler.fn_builder, size_t.bytes() * (u32::try_from(functions.len()).unwrap() + 1));

    let type_idx = func_compiler.fn_builder.ins().iconst(size_t, 3);

    func_compiler.fn_builder.ins().stack_store(type_idx, slot, 0);

    println!("storing {type_idx} at 0 vtable offset");

    for (i, (_, (_, func_ref, _))) in functions.values().enumerate() {
        let value = func_compiler.fn_builder.ins().func_addr(size_t, *func_ref);

        println!(
            "storing {value} at {} vtable offset",
            size_t.bytes().cast_signed() * (i32::try_from(i).unwrap() + 1)
        );

        func_compiler
            .fn_builder
            .ins()
            .stack_store(value, slot, size_t.bytes().cast_signed() * (i32::try_from(i).unwrap() + 1));
    }

    let ptr = func_compiler.fn_builder.ins().stack_addr(size_t, slot, 0);
    let functions = (ptr, functions);

    func_compiler.compiler.vtables.insert(draw_ctx_ty, VTable::from_iter([(None, functions)]));
}

#[derive(Debug)]
#[allow(dead_code)]
enum Op {
    DrawRect { x: f32, y: f32, width: f32, height: f32, color: u32 },
}

struct DrawContext {
    operations: Vec<Op>,
}

impl DrawContext {
    fn draw_rect(&mut self, x: f32, y: f32, width: f32, height: f32, color: u32) {
        println!("i was called! >:3 {}", self.operations.len());

        self.operations.push(Op::DrawRect { x, y, width, height, color });
    }
}

const PROGRAM: &str = r#"
enum Option<T> {
    Some { value: T },
    None
}

trait Iterable<T> {
    fn iter(self) -> T;
}

trait Iterator<T> {
    fn next(self) -> Option<T>;
}

impl<T> T[] {
    fn size(self) -> uint_size {
        get_size(self)
    }
}

struct ArrayIter<T> {
    value: T[],
    index: uint_size
}

impl<T> trait Iterator<T> for ArrayIter<T> {
    fn next(self) -> Option<T> {
        if self.index == get_size(self.value) {
            Option::None<T>
        } else {
            const returned = self.value[self.index];

            self.index = self.index + 1uint_size;

            Option::Some<T> {
                value: returned
            }
        }
    }
}

impl<T> trait Iterable<ArrayIter<T>> for T[] {
    fn iter(self) -> ArrayIter<T> {
        ArrayIter {
            value: self,
            index: 0
        }
    }
}

struct Constraints {
    min_width: float = 0.0,
    max_width: float = 0.0,
    min_height: float = 0.0,
    max_height: float = 0.0,
}

trait Placeable {
    fn get_width(self) -> float;
    fn get_height(self) -> float;

    fn get_measured_width(self) -> float;
    fn get_measured_height(self) -> float;

    fn place(self, x: float, y: float);
    fn render(self, ctx: DrawContext);
}

trait Measurable {
    fn measure(self, constraints: Constraints) -> Placeable;
}

declare Rectangle {
    x: float,
    y: float,
    width: float,
    height: float,
    color: uint32
}

impl trait Placeable for Rectangle {
    fn get_width(self) -> float {
        self.width
    }

    fn get_height(self) -> float {
        self.height
    }

    fn get_measured_width(self) -> float {
        self.width
    }

    fn get_measured_height(self) -> float {
        self.height
    }


    fn place(self, x: float, y: float) {
        self.x = x;
        self.y = y;
    }

    fn render(self, ctx: DrawContext) {
        println_str("rectangle");

        ctx.draw_rect(self.x, self.y, self.width, self.height, self.color);
    }
}

declare Container {
    vertical: boolean,
    width: float = 0.0,
    height: float = 0.0,
    children: Placeable[]
}

impl trait Placeable for Container {
    fn get_width(self) -> float {
        self.width
    }

    fn get_height(self) -> float {
        self.height
    }

    fn get_measured_width(self) -> float {
        self.width
    }

    fn get_measured_height(self) -> float {
        self.height
    }

    fn place(self, x: float, y: float) {
        let offset = if self.vertical == true {
            y
        } else {
            x
        };

        let iterator = ArrayIter<Placeable> {
            value: self.children,
            index: 0
        };

        while iterator.next() is Option::Some { value } {
            if self.vertical == true {
                value.place(x, offset);
            } else {
                value.place(offset, y);
            }

            if value is Rectangle rect {
                offset = offset + if self.vertical == true { rect.height } else { rect.width };
            }
        }
    }

    fn render(self, ctx: DrawContext) {
        let iterator = ArrayIter<Placeable> {
            value: self.children,
            index: 0
        };

        while iterator.next() is Option::Some { value } {
            value.render(ctx);
        }
    }
}

const contained = Container {
    vertical: true,
    
    Rectangle {
        x: 0.0,
        y: 0.0,
        width: 128.0,
        height: 256.0,
        color: 4278255360uint32
    }
    
    Rectangle {
        x: 24.0,
        y: 0.0,
        width: 256.0,
        height: 128.0,
        color: 4278255360uint32
    }
};

println_str("hm?");

contained.place(0.0, 0.0);
contained.render(context);

println_str("finish");

struct TestDefaults {
    try_to_do: int64 = 5int64
}

const ooo = TestDefaults {};

println(ooo.try_to_do);

declare TestDefaultsC {
    try_to_do: int64 = 5int64
}

const oooc = TestDefaultsC {};

println(1int64);
println(oooc.try_to_do);
println(2int64);

"#;

fn main() {
    let mut compiler = Compiler::with_symbols(vec![("DrawContext_draw_rect", DrawContext::draw_rect as *const u8)]);
    let mut provider = compiler.start_compiling();
    let mut compiler = provider.provide();
    let mut draw_ctx = DrawContext { operations: Vec::new() };

    add_builtins(&mut compiler, &mut draw_ctx);

    compiler.fn_builder.func.collect_debug_info();

    match compiler.compile_program_text(PROGRAM) {
        Ok(main_id) => {
            provider.compiler.jit.module.define_function(main_id, &mut provider.ctx).unwrap();
            provider.compiler.jit.module.clear_context(&mut provider.ctx);
            provider.compiler.jit.module.finalize_definitions().unwrap();

            let code = provider.compiler.jit.module.get_finalized_function(main_id);

            unsafe { std::mem::transmute::<*const u8, fn()>(code)() };

            println!("{:#?}", draw_ctx.operations);
        }
        Err(e) => match e {
            mollie_compiler::CompileError::Parse(parse_error) => Report::build(ariadne::ReportKind::Error, ("<anonymous>", 0..PROGRAM.len()))
                .with_message(parse_error.0)
                .with_label(Label::new(("<anonymous>", parse_error.1.unwrap_or_default().start..parse_error.1.unwrap_or_default().end)).with_message("here"))
                .finish()
                .eprint(("<anonymous>", Source::from(PROGRAM)))
                .unwrap(),
            e => println!("{e}"),
        },
    }
}
