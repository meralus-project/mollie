use mollie_lexer::Token;
use mollie_shared::Positioned;

use crate::{Expr, Ident, LiteralExpr, Parse, ParseResult, Parser, TypePathExpr, TypePathSegment};

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub struct NameValuePattern {
    pub name: Positioned<Ident>,
    pub value: Option<Positioned<IsPattern>>,
}

impl Parse for NameValuePattern {
    fn parse(parser: &mut Parser) -> ParseResult<Positioned<Self>> {
        let name = Ident::parse(parser)?;
        let value = parser.try_consume_then(&Token::Colon, IsPattern::parse)?;

        Ok(if let Some(value) = &value { name.between(value) } else { name.span }.wrap(Self { name, value }))
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub enum TypePattern {
    Name(Ident),
    Values(Vec<Positioned<NameValuePattern>>),
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub enum IsPattern {
    Literal(LiteralExpr),
    Type {
        ty: Positioned<TypePathExpr>,
        pattern: Positioned<TypePattern>,
    },
}

impl Parse for IsPattern {
    fn parse(parser: &mut Parser) -> ParseResult<Positioned<Self>> {
        LiteralExpr::parse(parser).map(|v| v.map(Self::Literal)).or_else(|_| {
            let ty = TypePathExpr::parse(TypePathSegment::parse_from(Ident::parse(parser)?, parser, false)?, parser, false)?;

            if parser.check(&Token::BraceOpen) {
                let values = parser.consume_separated_in(&Token::Comma, &Token::BraceOpen, &Token::BraceClose)?;

                Ok(ty.between(&values).wrap(Self::Type {
                    ty,
                    pattern: values.map(TypePattern::Values),
                }))
            } else {
                let name = Ident::parse(parser)?;

                Ok(ty.between(&name).wrap(Self::Type {
                    ty,
                    pattern: name.map(TypePattern::Name),
                }))
            }
        })
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub struct IsExpr {
    pub target: Box<Positioned<Expr>>,
    pub pattern: Positioned<IsPattern>,
}
