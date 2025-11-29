// use mollie_shared::Span;

use serde::Serialize;

use crate::{IntoTypedAST, TypeChecker, Typed, ExprRef};

#[derive(Debug, Serialize)]
#[serde(tag = "type", content = "data")]
#[serde(rename_all = "kebab-case")]
pub enum Stmt {
    Expr(ExprRef),
    VariableDecl {},
}

// impl IntoTypedAST<Stmt> for mollie_parser::Stmt {
//     fn into_typed_ast(self, checker: &mut TypeChecker, span: Span) -> Result<Typed<Stmt>, ()> {
//         use mollie_parser::Stmt::*;

//         match self {
//             Expression(value) => value.into_typed_ast(checker, span).map(|value| value.map(Self::Expression)),
//             VariableDecl(value) => value.into_typed_ast(checker, span).map(|value| value.map(Self::VariableDecl)),
//             StructDecl(value) => value.into_typed_ast(checker, span).map(|value| value.map(Self::StructDecl)),
//             ComponentDecl(value) => value.into_typed_ast(checker, span).map(|value| value.map(Self::ComponentDecl)),
//             TraitDecl(value) => value.into_typed_ast(checker, span).map(|value| value.map(Self::TraitDecl)),
//             EnumDecl(value) => value.into_typed_ast(checker, span).map(|value| value.map(Self::EnumDecl)),
//             FuncDecl(value) => value.into_typed_ast(checker, span).map(|value| value.map(Self::FuncDecl)),
//             Impl(value) => value.into_typed_ast(checker, span).map(|value| value.map(Self::Impl)),
//         }
//     }
// }
