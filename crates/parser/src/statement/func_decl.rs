use mollie_lexer::Token;
use mollie_shared::Positioned;

use crate::{BlockExpr, Ident, Parse, ParseResult, Parser, ty::Type};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub enum FuncVis {
    Public,
}

impl Parse for FuncVis {
    fn parse(parser: &mut Parser) -> ParseResult<Positioned<Self>> {
        let vis = parser.consume(&Token::Public)?;

        Ok(vis.wrap(Self::Public))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct Argument {
    pub name: Positioned<Ident>,
    pub ty: Positioned<Type>,
}

impl Parse for Argument {
    fn parse(parser: &mut Parser) -> ParseResult<Positioned<Self>> {
        parser.verify_if(Token::is_ident)?;
        parser.verify2(&Token::Colon)?;

        let name = Ident::parse(parser)?;

        parser.consume(&Token::Colon)?;

        let ty = Type::parse(parser)?;

        Ok(name.between(&ty).wrap(Self { name, ty }))
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub struct FuncDecl {
    pub func_vis: Option<Positioned<FuncVis>>,
    pub name: Positioned<Ident>,
    pub args: Vec<Positioned<Argument>>,
    pub returns: Option<Positioned<Type>>,
    pub body: Positioned<BlockExpr>,
}

impl Parse for FuncDecl {
    fn parse(parser: &mut Parser) -> ParseResult<Positioned<Self>> {
        let start = parser.consume(&Token::Fn)?;

        let func_vis = FuncVis::parse(parser).ok();

        let name = Ident::parse(parser)?;

        parser.consume(&Token::ParenOpen)?;

        let mut args = Vec::new();

        while !parser.check(&Token::ParenClose) {
            if !args.is_empty() {
                parser.consume(&Token::Comma)?;
            }

            if parser.check(&Token::ParenClose) {
                break;
            }

            args.push(Argument::parse(parser)?);
        }

        parser.consume(&Token::ParenClose)?;

        let returns = if parser.try_consume(&Token::Arrow) {
            Some(Type::parse(parser)?)
        } else {
            None
        };

        let body = BlockExpr::parse(parser)?;

        Ok(start.between(&body).wrap(Self {
            func_vis,
            name,
            args,
            returns,
            body,
        }))
    }
}
