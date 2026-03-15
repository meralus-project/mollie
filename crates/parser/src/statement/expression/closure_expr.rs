use mollie_lexer::Token;
use mollie_shared::Positioned;

use crate::{BlockExpr, Ident, Parse, ParseResult, Parser};

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub struct ClosureExpr {
    pub args: Positioned<Vec<Positioned<Ident>>>,
    pub body: Positioned<BlockExpr>,
}

impl Parse for ClosureExpr {
    fn parse(parser: &mut Parser) -> ParseResult<Positioned<Self>> {
        let args = if let Ok(token) = parser.consume(&Token::OrOr) {
            token.span.wrap(Vec::new())
        } else {
            parser.consume_separated_in(&Token::Comma, &Token::Or, &Token::Or)?
        };

        let body = BlockExpr::parse(parser)?;

        Ok(args.between(&body).wrap(Self { args, body }))
    }
}
