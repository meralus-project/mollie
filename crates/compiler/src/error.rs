use cranelift::module::ModuleError;
use mollie_shared::Positioned;
use mollie_typing::TypeError;

use crate::FuncCompilerError;

pub type CompileResult<T> = Result<T, CompileError>;

#[derive(Debug)]
pub enum CompileError {
    ExpectedAdt,
    FuncCompiler(FuncCompilerError),
    AdtArgsCount { expected: usize, found: usize },

    Type(Vec<Positioned<TypeError>>),
    Module(ModuleError),
}

impl From<ModuleError> for CompileError {
    fn from(error: ModuleError) -> Self {
        Self::Module(error)
    }
}

impl From<FuncCompilerError> for CompileError {
    fn from(value: FuncCompilerError) -> Self {
        Self::FuncCompiler(value)
    }
}
