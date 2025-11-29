use mollie_lexer::Token;
use mollie_shared::Positioned;

use crate::{BlockExpr, Expr, Ident, Parse, ParseResult, Parser};

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub struct ForInExpr {
    pub name: Positioned<Ident>,
    pub target: Box<Positioned<Expr>>,
    pub block: Positioned<BlockExpr>,
}

impl Parse for ForInExpr {
    fn parse(parser: &mut Parser) -> ParseResult<Positioned<Self>> {
        let start = parser.consume(&Token::For)?;
        let name = Ident::parse(parser)?;

        parser.consume(&Token::In)?;

        let target = Expr::parse(parser)?;
        let block = BlockExpr::parse(parser)?;

        Ok(start.between(&block).wrap(Self {
            name,
            target: Box::new(target),
            block,
        }))
    }
}
