use std::{error::Error, fmt, num::TryFromIntError};

use cranelift::module::ModuleError;
use mollie_parser::ParseError;

pub type CompileResult<T = ()> = Result<T, CompileError>;

#[derive(Debug)]
pub enum CompileError {
    Parse(ParseError),
    VariableNotFound { name: String },
    IntCast(TryFromIntError),
    Module(ModuleError),
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parse(error) => error.fmt(f),
            Self::VariableNotFound { name } => write!(f, "there's no variable called {name}"),
            Self::IntCast(error) => error.fmt(f),
            Self::Module(error) => error.fmt(f),
        }
    }
}

impl Error for CompileError {}

impl From<TryFromIntError> for CompileError {
    fn from(value: TryFromIntError) -> Self {
        Self::IntCast(value)
    }
}
