use cranelift::{
    codegen::ir::StackSlot,
    prelude::{FunctionBuilder, StackSlotData, StackSlotKind},
};

pub fn stack_alloc(fn_builder: &mut FunctionBuilder, size: u32) -> StackSlot {
    fn_builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, size, 0))
}
