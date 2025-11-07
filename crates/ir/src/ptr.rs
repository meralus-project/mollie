use cranelift::{
    codegen::ir,
    prelude::{FunctionBuilder, InstBuilder, isa::TargetIsa},
};

use crate::stack_alloc;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FatPtr;

impl FatPtr {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(isa: &dyn TargetIsa, fn_builder: &mut FunctionBuilder, ptr: ir::Value, metadata: ir::Value) -> ir::Value {
        let ptr_type = isa.pointer_type();
        let slot = stack_alloc(fn_builder, ptr_type.bytes() * 2);

        fn_builder.ins().stack_store(ptr, slot, 0);
        fn_builder.ins().stack_store(metadata, slot, ptr_type.bytes().cast_signed());

        fn_builder.ins().stack_addr(ptr_type, slot, 0)
    }

    pub fn get_ptr(isa: &dyn TargetIsa, fn_builder: &mut FunctionBuilder, fat_ptr: ir::Value) -> ir::Value {
        let ptr_type = isa.pointer_type();

        fn_builder.ins().load(ptr_type, ir::MemFlags::trusted(), fat_ptr, 0)
    }

    pub fn get_metadata(isa: &dyn TargetIsa, fn_builder: &mut FunctionBuilder, fat_ptr: ir::Value) -> ir::Value {
        let ptr_type = isa.pointer_type();

        fn_builder
            .ins()
            .load(ptr_type, ir::MemFlags::trusted(), fat_ptr, ptr_type.bytes().cast_signed())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VTablePtr;

impl VTablePtr {
    pub fn get_type_idx(isa: &dyn TargetIsa, fn_builder: &mut FunctionBuilder, vtable_ptr: ir::Value) -> ir::Value {
        let ptr_type = isa.pointer_type();

        fn_builder.ins().load(ptr_type, ir::MemFlags::trusted(), vtable_ptr, 0)
    }

    pub fn get_func_ptr(isa: &dyn TargetIsa, fn_builder: &mut FunctionBuilder, vtable_ptr: ir::Value, func_idx: u32) -> ir::Value {
        let ptr_type = isa.pointer_type();

        fn_builder
            .ins()
            .load(ptr_type, ir::MemFlags::trusted(), vtable_ptr, (ptr_type.bytes() * (func_idx + 1)).cast_signed())
    }
}
