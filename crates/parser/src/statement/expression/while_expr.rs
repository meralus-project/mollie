use mollie_lexer::Token;
use mollie_shared::Positioned;

use crate::{BlockExpr, Expr, Parse, ParseResult, Parser};

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub struct WhileExpr {
    pub condition: Box<Positioned<Expr>>,
    pub block: Positioned<BlockExpr>,
}

impl Parse for WhileExpr {
    fn parse(parser: &mut Parser) -> ParseResult<Positioned<Self>> {
        let start = parser.consume(&Token::While)?;

        let condition = Expr::parse(parser)?;
        let block = BlockExpr::parse(parser)?;

        Ok(start.between(&block).wrap(Self {
            condition: Box::new(condition),
            block,
        }))
    }
}
