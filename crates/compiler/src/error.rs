use cranelift::module::ModuleError;
use mollie_parser::ParseError;

pub type CompileResult<T> = Result<T, CompileError>;

#[derive(Debug)]
pub enum CompileError {
    ExpectedAdt,
    AdtArgsCount { expected: usize, found: usize },

    Parse(ParseError),
    Module(ModuleError),
}

impl From<ParseError> for CompileError {
    fn from(error: ParseError) -> Self {
        Self::Parse(error)
    }
}

impl From<ModuleError> for CompileError {
    fn from(error: ModuleError) -> Self {
        Self::Module(error)
    }
}
