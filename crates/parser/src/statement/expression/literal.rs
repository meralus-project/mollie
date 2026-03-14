use std::{
    hash::{Hash, Hasher},
    mem::discriminant,
};

use mollie_lexer::{NumberToken, Token};
use mollie_shared::Positioned;

use crate::{Parse, ParseResult, Parser};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Number {
    I64(i64),
    F32(f32),
}

impl Hash for Number {
    fn hash<H: Hasher>(&self, state: &mut H) {
        discriminant(self).hash(state);

        match self {
            Self::I64(value) => value.hash(state),
            Self::F32(value) => value.to_bits().hash(state),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub enum SizeType {
    Pixel,
    Percent,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub enum LiteralExpr {
    Number(Positioned<Number>, Option<Positioned<String>>),
    Boolean(bool),
    String(String),
}

impl Parse for LiteralExpr {
    fn parse(parser: &mut Parser) -> ParseResult<Positioned<Self>> {
        parser
            .consume_if(|token| matches!(token, Token::Boolean(_) | Token::Number(..) | Token::String(_) | Token::Null))
            .map(|token| {
                token.span.wrap(match token.value {
                    Token::Boolean(value) => Self::Boolean(value),
                    Token::Number(value, postfix) => Self::Number(
                        value.map(|value| match value {
                            NumberToken::Float(value) => Number::F32(value),
                            NumberToken::Int(value) => Number::I64(value),
                        }),
                        postfix,
                    ),
                    Token::String(value) => Self::String(value),
                    _ => unreachable!(),
                })
            })
    }
}
