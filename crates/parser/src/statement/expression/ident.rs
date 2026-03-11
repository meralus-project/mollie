use std::{
    borrow::Borrow,
    fmt,
    ops::{Deref, DerefMut},
};

use equivalent::Equivalent;
use mollie_lexer::Token;
use mollie_shared::Positioned;

use crate::{Parse, ParseResult, Parser};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct Ident(pub String);

impl Ident {
    pub fn new<T: Into<String>>(value: T) -> Self {
        Self(value.into())
    }
}

impl Parse for Ident {
    fn parse(parser: &mut Parser) -> ParseResult<Positioned<Self>> {
        parser.consume_if(Token::is_ident).map(|value| value.map(Token::unwrap_ident).map(Self))
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Deref for Ident {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Ident {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        self.0.as_str()
    }
}

impl AsRef<String> for Ident {
    fn as_ref(&self) -> &String {
        &self.0
    }
}

impl Borrow<str> for Ident {
    fn borrow(&self) -> &str {
        self.0.borrow()
    }
}

impl Borrow<String> for Ident {
    fn borrow(&self) -> &String {
        &self.0
    }
}

impl Equivalent<String> for Ident {
    fn equivalent(&self, key: &String) -> bool {
        PartialEq::eq(&self.0, key)
    }
}

impl From<&Ident> for String {
    fn from(value: &Ident) -> Self {
        value.0.clone()
    }
}
