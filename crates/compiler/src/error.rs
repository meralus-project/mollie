use cranelift::module::ModuleError;
use mollie_shared::Positioned;
use mollie_typed_ast::TypeError;

pub type CompileResult<T> = Result<T, CompileError>;

#[derive(Debug)]
pub enum CompileError {
    ExpectedAdt,
    AdtArgsCount { expected: usize, found: usize },

    Type(Vec<Positioned<TypeError>>),
    Module(ModuleError),
}

impl From<ModuleError> for CompileError {
    fn from(error: ModuleError) -> Self {
        Self::Module(error)
    }
}
