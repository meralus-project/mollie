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
        .var("get_type_idx", TypeVariant::function(false, [TypeVariant::any()], TypeVariant::usize()));
    func_compiler
        .compiler
        .var("get_size", TypeVariant::function(false, [TypeVariant::any()], TypeVariant::usize()));

    let draw_ctx_ty = TypeVariant::structure::<String, Type, _>([]);

    let var = func_compiler
        .fn_builder
        .declare_var(draw_ctx_ty.as_ir_type(func_compiler.compiler.jit.module.isa()));

    func_compiler.compiler.var("context", draw_ctx_ty.clone());
    func_compiler.compiler.variables.insert("context".to_string(), var);

    let value = func_compiler.fn_builder.ins().iconst(
        func_compiler.compiler.jit.module.isa().pointer_type(),
        std::ptr::from_mut::<DrawContext>(context).addr().cast_signed() as i64,
    );

    func_compiler.fn_builder.def_var(var, value);

    let draw_rect = {
        let mut sig = func_compiler.compiler.jit.module.make_signature();

        sig.params.push(AbiParam::new(func_compiler.compiler.jit.module.isa().pointer_type()));
        sig.params.push(AbiParam::new(types::F32));
        sig.params.push(AbiParam::new(types::F32));
        sig.params.push(AbiParam::new(types::F32));
        sig.params.push(AbiParam::new(types::F32));
        sig.params.push(AbiParam::new(types::I32));

        let func = func_compiler
            .compiler
            .jit
            .module
            .declare_function("DrawContext_draw_rect", Linkage::Import, &sig)
            .unwrap();
        let sig = func_compiler.fn_builder.import_signature(sig);
        let func = func_compiler.compiler.jit.module.declare_func_in_func(func, func_compiler.fn_builder.func);

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
            (sig, func),
        )
    };

    let functions = IndexMap::from_iter([(String::from("draw_rect"), draw_rect)]);

    let size_t = func_compiler.compiler.jit.module.isa().pointer_type();
    let slot = stack_alloc(&mut func_compiler.fn_builder, size_t.bytes() * (u32::try_from(functions.len()).unwrap() + 1));

    let type_idx = func_compiler.fn_builder.ins().iconst(size_t, 3);

    func_compiler.fn_builder.ins().stack_store(type_idx, slot, 0);

    println!("storing {type_idx} at 0 vtable offset");

    for (i, (_, (_, func_ref))) in functions.values().enumerate() {
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

enum Op {
    DrawRect { x: f32, y: f32, width: f32, height: f32, color: u32 },
}

struct DrawContext {
    operations: Vec<Op>,
}

impl DrawContext {
    fn draw_rect(&mut self, x: f32, y: f32, width: f32, height: f32, color: u32) {
        println!("i was called! >:3");

        self.operations.push(Op::DrawRect { x, y, width, height, color });
    }
}

const PROGRAM: &str = r#"
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

context.draw_rect(0.0, 0.0, 24.0, 24.0, 65280uint32);
"#;

fn main() {
    let mut compiler = Compiler::with_symbols(vec![("DrawContext_draw_rect", DrawContext::draw_rect as *const u8)]);
    let mut provider = compiler.start_compiling();
    let mut compiler = provider.provide();
    let mut draw_ctx = DrawContext { operations: Vec::new() };

    add_builtins(&mut compiler, &mut draw_ctx);

    let main_id = compiler.compile_program_text(PROGRAM).unwrap();

    provider.compiler.jit.module.define_function(main_id, &mut provider.ctx).unwrap();
    provider.compiler.jit.module.clear_context(&mut provider.ctx);
    provider.compiler.jit.module.finalize_definitions().unwrap();

    let code = provider.compiler.jit.module.get_finalized_function(main_id);

    unsafe { std::mem::transmute::<*const u8, fn()>(code)() };
}
