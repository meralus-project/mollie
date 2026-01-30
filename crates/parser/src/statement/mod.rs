mod component_decl;
mod enum_decl;
mod expression;
mod func_decl;
mod implementation;
mod import;
mod struct_decl;
mod trait_decl;
mod variable_decl;

use mollie_lexer::Token;
use mollie_shared::Positioned;

pub use self::{
    component_decl::{ComponentDecl, ComponentProperty},
    enum_decl::EnumDecl,
    expression::*,
    func_decl::{Argument, FuncDecl},
    implementation::{Impl, ImplFunction},
    import::{Import, ImportKind},
    struct_decl::{Property, StructDecl},
    trait_decl::{TraitDecl, TraitFuncArgument, TraitFunction},
    variable_decl::VariableDecl,
};
use super::{ParseResult, Parser};
use crate::{Parse, ParseError};

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub struct Attribute {
    pub name: Positioned<Ident>,
    pub value: Option<Positioned<LiteralExpr>>,
}

impl Parse for Attribute {
    fn parse(parser: &mut Parser) -> ParseResult<Positioned<Self>> {
        let start = parser.consume(&Token::Attr)?;

        parser.consume(&Token::BracketOpen)?;

        let name = Ident::parse(parser)?;
        let value = if parser.try_consume(&Token::Eq) {
            Some(LiteralExpr::parse(parser)?)
        } else {
            None
        };

        let end = parser.consume(&Token::BraceClose)?;

        Ok(start.between(&end).wrap(Self { name, value }))
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub enum Stmt {
    Expression(Expr),
    VariableDecl(VariableDecl),
    StructDecl(StructDecl),
    ComponentDecl(ComponentDecl),
    TraitDecl(TraitDecl),
    EnumDecl(EnumDecl),
    FuncDecl(FuncDecl),
    Impl(Impl),
    Import(Import),
}

impl Parse for Stmt {
    fn parse(parser: &mut Parser) -> ParseResult<Positioned<Self>> {
        let attributes = {
            let mut items = Vec::new();

            while parser.check(&Token::Attr) {
                items.push(Attribute::parse(parser)?);
            }

            items
        };

        match parser.peek().map(|v| &v.value) {
            Some(Token::Declare) => Ok(ComponentDecl::parse(parser, attributes)?.map(Self::ComponentDecl)),
            Some(Token::Struct) => Ok(StructDecl::parse(parser, attributes)?.map(Self::StructDecl)),
            Some(Token::Enum) => Ok(EnumDecl::parse(parser, attributes)?.map(Self::EnumDecl)),
            Some(Token::Let | Token::Const) => Ok(VariableDecl::parse(parser)?.map(Self::VariableDecl)),
            Some(Token::Impl) => Ok(Impl::parse(parser)?.map(Self::Impl)),
            Some(Token::Fn) => Ok(FuncDecl::parse(parser)?.map(Self::FuncDecl)),
            Some(Token::Trait) => Ok(TraitDecl::parse(parser)?.map(Self::TraitDecl)),
            Some(Token::Import) => Ok(Import::parse(parser)?.map(Self::Import)),
            Some(_) => Ok(Expr::parse(parser)?.map(Self::Expression)),
            None => Err(ParseError::new("unexpected <EOF>", None)),
        }
    }
}
