mod token;

use std::{
    iter::{self, Peekable},
    ops::Neg,
    str::Chars,
};

use mollie_shared::{Positioned, Span, SpanRange};

pub use crate::token::{NumberToken, Token};

pub struct Lexer;

impl Lexer {
    fn lex_reserved(ident: String) -> Token {
        match ident.as_str() {
            "true" => Token::Bool(true),
            "false" => Token::Bool(false),
            "self" => Token::This,
            "view" => Token::View,
            "inherits" => Token::Inherits,
            "as" => Token::As,
            "from" => Token::From,
            "struct" => Token::Struct,
            "enum" => Token::Enum,
            "import" => Token::Import,
            "switch" => Token::Switch,
            "func" => Token::Func,
            "trait" => Token::Trait,
            "impl" => Token::Impl,
            "const" => Token::Const,
            "let" => Token::Let,
            "while" => Token::While,
            "for" => Token::For,
            "public" => Token::Public,
            "postfix" => Token::Postfix,
            "in" => Token::In,
            "loop" => Token::Loop,
            "if" => Token::If,
            "else" => Token::Else,
            "is" => Token::Is,
            _ => Token::Ident(ident),
        }
    }

    fn lex_other(chars: &mut Peekable<Chars>, span: &mut Span, character: char) -> Option<Token> {
        match character {
            '[' => Some(Token::BracketOpen),
            ']' => Some(Token::BracketClose),
            '{' => Some(Token::BraceOpen),
            '}' => Some(Token::BraceClose),
            '(' => Some(Token::ParenOpen),
            ')' => Some(Token::ParenClose),
            ':' => {
                if chars.next_if_eq(&':').is_some() {
                    span.end += 1;
                    span.range.add_columns(1);

                    Some(Token::PathSep)
                } else {
                    Some(Token::Colon)
                }
            }
            ';' => Some(Token::Semi),
            '+' => {
                if chars.next_if_eq(&'=').is_some() {
                    span.end += 1;
                    span.range.add_columns(1);

                    Some(Token::PlusEq)
                } else {
                    Some(Token::Plus)
                }
            }
            '*' => {
                if chars.next_if_eq(&'=').is_some() {
                    span.end += 1;
                    span.range.add_columns(1);

                    Some(Token::StarEq)
                } else {
                    Some(Token::Star)
                }
            }
            '/' => {
                if chars.next_if_eq(&'/').is_some() {
                    let mut utf8size = 0;

                    while let Some(character) = chars.next_if(|character| character != &'\n') {
                        utf8size += character.len_utf8();
                    }

                    chars.next_if_eq(&'\n');

                    span.start = span.end;
                    span.end += utf8size + 2;
                    span.range.add_lines(1);
                    span.range.set_column(0);

                    None
                } else if chars.next_if_eq(&'=').is_some() {
                    span.end += 1;
                    span.range.add_columns(1);

                    Some(Token::SlashEq)
                } else {
                    Some(Token::Slash)
                }
            }
            '=' => {
                if chars.next_if_eq(&'=').is_some() {
                    span.end += 1;
                    span.range.add_columns(1);

                    Some(Token::EqEq)
                } else if chars.next_if_eq(&'>').is_some() {
                    span.end += 1;
                    span.range.add_columns(1);

                    Some(Token::FatArrow)
                } else {
                    Some(Token::Eq)
                }
            }
            '&' => {
                if chars.next_if_eq(&'&').is_some() {
                    span.end += 1;
                    span.range.add_columns(1);

                    Some(Token::AndAnd)
                } else if chars.next_if_eq(&'=').is_some() {
                    span.end += 1;
                    span.range.add_columns(1);

                    Some(Token::AndEq)
                } else {
                    Some(Token::And)
                }
            }
            '|' => {
                if chars.next_if_eq(&'|').is_some() {
                    span.end += 1;
                    span.range.add_columns(1);

                    Some(Token::OrOr)
                } else if chars.next_if_eq(&'=').is_some() {
                    span.end += 1;
                    span.range.add_columns(1);

                    Some(Token::OrEq)
                } else {
                    Some(Token::Or)
                }
            }
            '%' => Some(Token::Percent),
            '.' => {
                if chars.next_if_eq(&'.').is_some() {
                    span.end += 1;
                    span.range.add_columns(1);

                    Some(Token::DotDot)
                } else {
                    Some(Token::Dot)
                }
            }
            ',' => Some(Token::Comma),
            '!' => {
                if chars.next_if_eq(&'=').is_some() {
                    span.end += 1;
                    span.range.add_columns(1);

                    Some(Token::NotEq)
                } else {
                    Some(Token::Not)
                }
            }
            // '#' => Token::Pound,
            '@' => Some(Token::Attr),
            '?' => Some(Token::Question),
            '>' => {
                if chars.next_if_eq(&'=').is_some() {
                    span.end += 1;
                    span.range.add_columns(1);

                    Some(Token::GreaterEq)
                } else {
                    Some(Token::Greater)
                }
            }
            '<' => {
                if chars.next_if_eq(&'=').is_some() {
                    span.end += 1;
                    span.range.add_columns(1);

                    Some(Token::LessEq)
                } else {
                    Some(Token::Less)
                }
            }
            character => Some(Token::Unknown(character)),
        }
    }

    const fn len_utf8(char: char) -> u32 {
        const MAX_ONE_B: u32 = 0x80;
        const MAX_TWO_B: u32 = 0x800;
        const MAX_THREE_B: u32 = 0x10000;

        match char as u32 {
            ..MAX_ONE_B => 1,
            MAX_ONE_B..MAX_TWO_B => 2,
            MAX_TWO_B..MAX_THREE_B => 3,
            _ => 4,
        }
    }

    fn parse_string(chars: &mut Peekable<Chars>) -> (Token, u32, u32) {
        let mut size = 0;
        let mut utf8size = 0;
        let mut data = String::new();

        while let Some(character) = chars.next_if(|character| character != &'"') {
            size += 1;
            utf8size += Self::len_utf8(character);

            if character == '\\'
                && let Some(character) = chars.next()
            {
                size += 1;
                utf8size += Self::len_utf8(character);

                data.push(character);
            }

            data.push(character);
        }

        chars.next_if_eq(&'"');

        (Token::String(data), size, utf8size)
    }

    fn lex_number(chars: &mut Peekable<Chars>, tokens: &mut Vec<Positioned<Token>>, span: &mut Span, character: char, neg: bool) {
        if character == '0' && chars.next_if_eq(&'x').is_some() {
            let hex = iter::from_fn(|| chars.by_ref().next_if(char::is_ascii_hexdigit)).collect::<String>();

            span.start = span.end;
            span.end += hex.len() + 2;

            tokens.push(span.wrap(Token::Number(span.wrap(NumberToken::I64(i64::from_str_radix(&hex, 16).unwrap())), None)));
        } else {
            let mut size = 1;
            let mut value = String::from(character);

            while let Some(c) = chars.next_if(|c| c.is_ascii_digit() || matches!(c, '.' | '_')) {
                size += 1;

                if c != '_' {
                    value.push(c);
                }
            }

            span.start = span.end;
            span.end += value.len();
            span.range.start_column = span.range.end_column;
            span.range.end_column += size;

            let mut number = span.wrap(if value.contains('.') {
                NumberToken::F32(value.parse().unwrap())
            } else {
                NumberToken::I64(value.parse().unwrap())
            });

            if neg {
                match &mut number.value {
                    NumberToken::F32(value) => *value = value.neg(),
                    NumberToken::I64(value) => *value = value.neg(),
                }
            }

            let postfix = if let Some(c) = chars.next_if(char::is_ascii_alphabetic) {
                let mut size = 1;
                let mut postfix = String::from(c);

                while let Some(c) = chars.next_if(|c| c.is_ascii_alphanumeric() || c == &'_') {
                    size += 1;
                    postfix.push(c);
                }

                span.start = span.end;
                span.end += postfix.len();
                span.range.start_column = span.range.end_column;
                span.range.end_column += size;

                let postfix = span.wrap(postfix);

                Some(postfix)
            } else {
                None
            };

            tokens.push(
                number
                    .span
                    .between(postfix.as_ref().map_or(number.span, |postfix| postfix.span))
                    .wrap(Token::Number(number, postfix)),
            );
        }
    }

    /// # Panics
    ///
    /// Can panic if number failed to parse
    pub fn lex<T: AsRef<str>>(data: T) -> Vec<Positioned<Token>> {
        let mut tokens = vec![];
        let mut chars = data.as_ref().chars().peekable();
        let mut span = Span::new(0, 0, SpanRange::from_single(0, 0));

        while let Some(character) = chars.next() {
            match character {
                'A'..='Z' | 'a'..='z' => {
                    span.start = span.end;
                    span.range.start_column = span.range.end_column;
                    span.end += 1;
                    span.range.end_column += 1;

                    let mut ident = String::from(character);

                    while let Some(c) = chars.by_ref().next_if(|s| s.is_ascii_alphanumeric() || s == &'-' || s == &'_') {
                        span.end += 1;
                        span.range.end_column += 1;

                        ident.push(c);
                    }

                    tokens.push(span.wrap(Self::lex_reserved(ident)));
                }
                '0'..='9' => {
                    Self::lex_number(&mut chars, &mut tokens, &mut span, character, false);
                }
                '-' => {
                    if let Some(character) = chars.next_if(char::is_ascii_digit) {
                        Self::lex_number(&mut chars, &mut tokens, &mut span, character, true);
                    } else if chars.next_if_eq(&'>').is_some() {
                        span.start = span.end;
                        span.end += 2;

                        tokens.push(span.wrap(Token::Arrow));

                        span.range.add_columns(2);
                    } else if chars.next_if_eq(&'=').is_some() {
                        span.start = span.end;
                        span.end += 2;

                        tokens.push(span.wrap(Token::MinusEq));

                        span.range.add_columns(2);
                    } else {
                        span.start = span.end;
                        span.end += 1;

                        tokens.push(span.wrap(Token::Minus));

                        span.range.add_columns(1);
                    }
                }
                '"' => {
                    span.start = span.end;

                    let (value, size, utf8size) = Self::parse_string(&mut chars);

                    span.start = span.end;
                    span.end += utf8size as usize + 2;
                    span.range.end_column += size + 2;

                    tokens.push(span.wrap(value));

                    span.range.start_column += size + 2;
                }
                character => {
                    span.start = span.end;
                    span.end += 1;

                    if character.is_ascii_whitespace() {
                        if character == '\n' {
                            span.range.add_lines(1);
                            span.range.set_column(0);
                        } else {
                            span.range.add_columns(1);
                        }

                        continue;
                    }

                    if let Some(token) = Self::lex_other(&mut chars, &mut span, character) {
                        tokens.push(span.wrap(token));

                        span.range.add_columns(1);
                    }
                }
            }
        }

        span.start = span.end;
        span.range.start_column = span.range.end_column;

        tokens.push(span.wrap(Token::EOF));

        tokens
    }
}

#[cfg(test)]
mod tests {
    use mollie_shared::{Span, SpanRange};

    use crate::{Lexer, NumberToken, Token};

    fn assert_lex_single_eq(input: &str, output: &Token) {
        let lexed = Lexer::lex(input);

        assert!(!lexed.is_empty());
        assert_eq!(&lexed[0].value, output);
    }

    fn assert_lex_eq<T: IntoIterator<Item = (Token, Span)>>(input: &str, output: T) {
        let lexed = Lexer::lex(input);

        assert!(!lexed.is_empty());
        assert_eq!(lexed, output.into_iter().map(|(token, span)| span.wrap(token)).collect::<Vec<_>>());
    }

    #[test]
    fn test_number_parsing() {
        assert_lex_eq("bruh", [
            (Token::ident("bruh"), Span::new(0, 4, SpanRange::new(0, 0, 0, 4))),
            (Token::EOF, Span::new(4, 4, SpanRange::new(0, 4, 0, 4))),
        ]);

        assert_lex_eq("\"bruh\"", [
            (Token::String(String::from("bruh")), Span::new(0, 6, SpanRange::new(0, 0, 0, 6))),
            (Token::EOF, Span::new(6, 6, SpanRange::new(0, 6, 0, 6))),
        ]);

        assert_lex_eq("\"bruh\" \"bruh\"", [
            (Token::String(String::from("bruh")), Span::new(0, 6, SpanRange::new(0, 0, 0, 6))),
            (Token::String(String::from("bruh")), Span::new(7, 13, SpanRange::new(0, 7, 0, 13))),
            (Token::EOF, Span::new(13, 13, SpanRange::new(0, 13, 0, 13))),
        ]);

        assert_lex_single_eq(
            "123",
            &Token::Number(Span::new(0, 3, SpanRange::new(0, 0, 0, 3)).wrap(NumberToken::I64(123)), None),
        );

        assert_lex_single_eq(
            "123.0",
            &Token::Number(Span::new(0, 5, SpanRange::new(0, 0, 0, 5)).wrap(NumberToken::F32(123.0)), None),
        );

        assert_lex_single_eq(
            "123f32",
            &Token::Number(
                Span::new(0, 3, SpanRange::new(0, 0, 0, 3)).wrap(NumberToken::I64(123)),
                Some(Span::new(3, 8, SpanRange::new(0, 3, 0, 8)).wrap(String::from("f32"))),
            ),
        );

        assert_lex_single_eq(
            "123.0f32",
            &Token::Number(
                Span::new(0, 5, SpanRange::new(0, 0, 0, 5)).wrap(NumberToken::F32(123.0)),
                Some(Span::new(5, 10, SpanRange::new(0, 5, 0, 10)).wrap(String::from("f32"))),
            ),
        );
    }
}
