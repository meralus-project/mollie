use mollie_lexer::Token;
use mollie_shared::Positioned;

use crate::{Expr, Ident, Parse, ParseResult, Parser, Type};

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub struct VariableDecl {
    pub mutable: Option<Positioned<()>>,
    pub name: Positioned<Ident>,
    pub ty: Option<Positioned<Type>>,
    pub value: Positioned<Expr>,
}

impl Parse for VariableDecl {
    fn parse(parser: &mut Parser) -> ParseResult<Positioned<Self>> {
        let start = parser.consume_one_of(&[Token::Const, Token::Let])?;
        let start_span = start.span;
        let mutable = if start.value == Token::Let { Some(start.wrap(())) } else { None };
        let name = Ident::parse(parser)?;

        let ty = if parser.try_consume(&Token::Colon) {
            Some(Type::parse(parser)?)
        } else {
            None
        };

        parser.consume(&Token::Eq)?;

        let value = Expr::parse(parser)?;
        let end = parser.consume(&Token::Semi)?;

        Ok(start_span.between(end.span).wrap(Self { mutable, name, ty, value }))
    }
}
