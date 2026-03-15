use mollie_lexer::Token;
use mollie_shared::Positioned;

use crate::{Expr, ParseResult, Parser};

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub struct FuncCallExpr {
    pub function: Box<Positioned<Expr>>,
    pub args: Positioned<Vec<Positioned<Expr>>>,
}

impl FuncCallExpr {
    /// # Errors
    ///
    /// Returns error if parsing failed
    pub fn parse(parser: &mut Parser, target: Positioned<Expr>) -> ParseResult<Positioned<Self>> {
        let args = parser.consume_separated_in::<Expr>(&Token::Comma, &Token::ParenOpen, &Token::ParenClose)?;

        Ok(target.between(&args).wrap(Self {
            function: Box::new(target),
            args,
        }))
    }
}
