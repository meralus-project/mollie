mod ptr;
mod ty;
mod utils;

use std::fmt;

use cranelift::{
    codegen::ir,
    jit::{JITBuilder, JITModule},
    module::{DataDescription, DataId, Module, ModuleResult, default_libcall_names},
    native,
    prelude::settings,
};

pub use self::{
    ptr::{FatPtr, VTablePtr},
    ty::{Array, Field, Struct, compile_constant},
    utils::stack_alloc,
};

pub type Symbol = (&'static str, *const u8);
pub struct CodeGenerator<M: Module> {
    pub module: M,
    pub data_desc: DataDescription,
}

impl<M: Module> fmt::Debug for CodeGenerator<M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("JitCompiler").field("data_desc", &self.data_desc).finish_non_exhaustive()
    }
}

impl CodeGenerator<JITModule> {
    pub fn new<I: IntoIterator<Item = Symbol>>(symbols: I, flags: settings::Flags) -> Self {
        let mut builder = JITBuilder::with_isa(native::builder().unwrap().finish(flags).unwrap(), default_libcall_names());

        for (name, ptr) in symbols {
            builder.symbol(name, ptr);
        }

        Self {
            module: JITModule::new(builder),
            data_desc: DataDescription::new(),
        }
    }
}

impl<M: Module> CodeGenerator<M> {
    pub fn static_data<T: Into<Box<[u8]>>>(&mut self, data: T) -> ModuleResult<DataId> {
        let data = data.into();

        self.data_desc.define(data);

        let id = self.module.declare_anonymous_data(false, false).unwrap();

        self.module.define_data(id, &self.data_desc).unwrap();
        self.data_desc.clear();

        Ok(id)
    }

    pub fn static_zeroed(&mut self, size: usize) -> ModuleResult<DataId> {
        self.data_desc.define_zeroinit(size);

        let id = self.module.declare_anonymous_data(true, false)?;

        self.module.define_data(id, &self.data_desc)?;
        self.data_desc.clear();

        Ok(id)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MollieType {
    Regular(ir::Type),
    Fat(ir::Type, ir::Type),
}

impl MollieType {
    pub fn bytes(&self) -> u32 {
        match self {
            Self::Regular(ty) => ty.bytes(),
            Self::Fat(ty, metadata_ty) => ty.bytes() + metadata_ty.bytes(),
        }
    }

    pub fn add_to_params(self, params: &mut Vec<ir::AbiParam>) {
        match self {
            Self::Regular(ty) => params.push(ir::AbiParam::new(ty)),
            Self::Fat(ty, metadata_ty) => {
                params.push(ir::AbiParam::new(ty));
                params.push(ir::AbiParam::new(metadata_ty));
            }
        }
    }

    /// Returns `true` if the mollie type is [`Fat`].
    ///
    /// [`Fat`]: MollieType::Fat
    #[must_use]
    pub const fn is_fat(&self) -> bool {
        matches!(self, Self::Fat(..))
    }
}

impl Default for MollieType {
    fn default() -> Self {
        Self::Regular(ir::Type::default())
    }
}
