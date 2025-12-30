use mollie_lexer::Token;
use mollie_shared::Positioned;

use crate::{Ident, Parse, ParseResult, Parser};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub enum ImportKind {
    Partial(Positioned<Vec<Positioned<Ident>>>), // import { a, b } from "module";
    Full(Positioned<Ident>),                     // import "module" as module;
    Eval,                                        // import "module";
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct Import {
    pub kind: ImportKind,
    pub path: Positioned<String>,
}

impl Parse for Import {
    fn parse(parser: &mut Parser) -> ParseResult<Positioned<Self>> {
        let start = parser.consume(&Token::Import)?;

        if parser.check(&Token::BraceOpen) {
            let values = parser.consume_separated_in(&Token::Comma, &Token::BraceOpen, &Token::BraceClose)?;

            parser.consume(&Token::From)?;

            let path = parser.consume_if(Token::is_string).map(|token| match token.value {
                Token::String(data) => token.span.wrap(data),
                _ => unreachable!(),
            })?;

            Ok(start.between(&path).wrap(Self {
                kind: ImportKind::Partial(values),
                path,
            }))
        } else {
            let path = parser.consume_if(Token::is_string).map(|token| match token.value {
                Token::String(data) => token.span.wrap(data),
                _ => unreachable!(),
            })?;

            if parser.try_consume(&Token::As) {
                let name = Ident::parse(parser)?;

                Ok(start.between(&path).wrap(Self {
                    kind: ImportKind::Full(name),
                    path,
                }))
            } else {
                Ok(start.between(&path).wrap(Self { kind: ImportKind::Eval, path }))
            }
        }
    }
}
