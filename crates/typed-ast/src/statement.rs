use serde::Serialize;

use crate::{BlockRef, ExprRef};

#[derive(Debug, Serialize)]
#[serde(tag = "type", content = "data")]
#[serde(rename_all = "kebab-case")]
pub enum Import {
    Partial(Vec<String>),
    Full(String),
    Eval(BlockRef),
}

#[derive(Debug, Serialize)]
#[serde(tag = "type", content = "data")]
#[serde(rename_all = "kebab-case")]
pub enum Stmt {
    Expr(ExprRef),
    VariableDecl { name: String, value: ExprRef },
    Import(Import),
}
