use mollie_lexer::Token;
use mollie_shared::Positioned;

use crate::{CustomType, Expr, Ident, LiteralExpr, Parse, ParseError, ParseResult, Parser};

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub struct NameValuePattern {
    pub name: Positioned<Ident>,
    pub value: Option<Positioned<IsPattern>>,
}

impl Parse for NameValuePattern {
    fn parse(parser: &mut Parser) -> ParseResult<Positioned<Self>> {
        let name = Ident::parse(parser)?;
        let value = if parser.try_consume(&Token::Colon) {
            Some(IsPattern::parse(parser)?)
        } else {
            None
        };

        Ok(if let Some(value) = &value { name.between(value) } else { name.span }.wrap(Self { name, value }))
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub enum IsPattern {
    Literal(LiteralExpr),
    Enum {
        target: Positioned<Ident>,
        index: Positioned<CustomType>,
        values: Option<Positioned<Vec<Positioned<NameValuePattern>>>>,
    },
    TypeName {
        ty: Positioned<CustomType>,
        name: Positioned<Ident>,
    },
}

impl Parse for IsPattern {
    fn parse(parser: &mut Parser) -> ParseResult<Positioned<Self>> {
        LiteralExpr::parse(parser)
            .map(|v| v.map(Self::Literal))
            .or_else(|_| {
                parser.verify_if(Token::is_ident)?;
                parser.verify2(&Token::PathSep)?;

                let target = Ident::parse(parser)?;

                parser.consume(&Token::PathSep)?;

                let index = CustomType::parse(parser)?;

                let values = parser.consume_separated_in(&Token::Comma, &Token::BraceOpen, &Token::BraceClose).ok();

                Ok(values
                    .as_ref()
                    .map_or_else(|| target.between(&index), |values| target.between(values))
                    .wrap(Self::Enum { target, index, values }))
            })
            .or_else(|_: ParseError| {
                let ty = CustomType::parse(parser)?;
                let name = Ident::parse(parser)?;

                Ok(ty.between(&name).wrap(Self::TypeName { ty, name }))
            })
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub struct IsExpr {
    pub target: Box<Positioned<Expr>>,
    pub pattern: Positioned<IsPattern>,
}
