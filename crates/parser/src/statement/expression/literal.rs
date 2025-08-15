use mollie_lexer::Token;
use mollie_shared::Positioned;

use crate::{Parse, ParseResult, Parser};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Number {
    I64(i64),
    F32(f32),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
pub enum SizeType {
    Pixel,
    Percent,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum LiteralExpr {
    SizeUnit(Number, SizeType),
    Number(Number, Option<String>),
    Boolean(bool),
    String(String),
    Null,
}

impl Parse for LiteralExpr {
    fn parse(parser: &mut Parser) -> ParseResult<Positioned<Self>> {
        parser
            .consume_if(|token| {
                matches!(
                    token,
                    Token::Boolean(_) | Token::Integer(..) | Token::String(_) | Token::Float(..) | Token::Null
                )
            })
            .map(|token| {
                token.span.wrap(match token.value {
                    Token::Boolean(value) => Self::Boolean(value),
                    Token::Integer(value, postfix, _) => Self::Number(Number::I64(value), postfix),
                    Token::Float(value, postfix) => Self::Number(Number::F32(value), postfix),
                    Token::String(value) => Self::String(value),
                    Token::Null => Self::Null,
                    _ => unreachable!(),
                })
            })
    }
}
