mod array;
mod as_expr;
mod binary;
mod block;
mod call;
mod enum_path;
mod for_in_expr;
mod ident;
mod if_else;
mod index;
mod literal;
mod loop_expr;
mod node;
mod type_index;
mod while_expr;
mod closure_expr;

use mollie_lexer::Token;
use mollie_shared::{Operator, Positioned};

pub use self::{
    array::ArrayExpr,
    as_expr::{IsExpr, IsPattern, NameValuePattern},
    binary::BinaryExpr,
    block::{BlockExpr, parse_statements_until},
    call::FuncCallExpr,
    enum_path::EnumPathExpr,
    ident::Ident,
    if_else::IfElseExpr,
    index::{IndexExpr, IndexTarget},
    literal::{LiteralExpr, Number, SizeType},
    node::{NameValue, NodeExpr},
    type_index::TypeIndexExpr,
    while_expr::WhileExpr,
    closure_expr::ClosureExpr,
};
use crate::{Parse, ParseResult, Parser};

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
pub enum Precedence {
    PLowest,
    POr,
    PAnd,
    PEquals,
    PLessGreater,
    PSum,
    PProduct,
    PCall,
    PIndex,
    PCheck,
}

impl Precedence {
    const fn from_ref(token: &Token) -> (Self, Option<Operator>) {
        match &token {
            Token::AndAnd => (Self::PAnd, Some(Operator::And)),
            Token::OrOr => (Self::POr, Some(Operator::Or)),
            Token::Eq => (Self::PEquals, Some(Operator::Assign)),
            Token::EqEq => (Self::PEquals, Some(Operator::Equal)),
            Token::NotEq => (Self::PEquals, Some(Operator::NotEqual)),
            Token::Less => (Self::PLessGreater, Some(Operator::LessThan)),
            Token::Greater => (Self::PLessGreater, Some(Operator::GreaterThan)),
            Token::Plus => (Self::PSum, Some(Operator::Add)),
            Token::Minus => (Self::PSum, Some(Operator::Sub)),
            Token::Star => (Self::PProduct, Some(Operator::Mul)),
            Token::Slash => (Self::PProduct, Some(Operator::Div)),
            Token::ParenOpen => (Self::PCall, None),
            Token::BracketOpen | Token::Dot => (Self::PIndex, None),
            Token::Is => (Self::PCheck, None),
            _ => (Self::PLowest, None),
        }
    }
}

fn go_parse_pratt_expr(parser: &mut Parser, precedence: Precedence, left: Positioned<Expr>) -> ParseResult<Positioned<Expr>> {
    if let Some(value) = parser.peek() {
        let (p, _) = Precedence::from_ref(&value.value);

        match p {
            Precedence::PCheck if precedence < Precedence::PCheck => {
                parser.consume(&Token::Is)?;

                let pattern = IsPattern::parse(parser)?;

                let left = left.between(&pattern).wrap(Expr::Is(IsExpr {
                    target: Box::new(left),
                    pattern,
                }));

                go_parse_pratt_expr(parser, precedence, left)
            }
            Precedence::PCall if precedence < Precedence::PCall => {
                let left = FuncCallExpr::parse(parser, left)?.map(Expr::FunctionCall);

                go_parse_pratt_expr(parser, precedence, left)
            }
            Precedence::PIndex if precedence < Precedence::PIndex => {
                let left = IndexExpr::parse(parser, left)?.map(Expr::Index);

                go_parse_pratt_expr(parser, precedence, left)
            }
            ref peek_precedence if precedence < *peek_precedence => {
                let left = BinaryExpr::parse(parser, left)?.map(Expr::Binary);

                go_parse_pratt_expr(parser, precedence, left)
            }
            _ => Ok(left),
        }
    } else {
        Ok(left)
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub enum Expr {
    Literal(LiteralExpr),
    FunctionCall(FuncCallExpr),
    Node(NodeExpr),
    Index(IndexExpr),
    Binary(BinaryExpr),
    TypeIndex(TypeIndexExpr),
    Array(ArrayExpr),
    IfElse(IfElseExpr),
    While(WhileExpr),
    Block(BlockExpr),
    EnumPath(EnumPathExpr),
    Is(IsExpr),
    Closure(ClosureExpr),
    Ident(Ident),
    This,
}

impl Expr {
    fn parse_atom(parser: &mut Parser, is_limited_expr: bool) -> ParseResult<Positioned<Self>> {
        if parser.check(&Token::ParenOpen) {
            let (_, mut parser) = parser.split(&Token::ParenOpen, &Token::ParenClose)?;

            Self::parse(&mut parser)
        } else if is_limited_expr {
            LiteralExpr::parse(parser)
                .map(|v| v.map(Self::Literal))
                .or_else(|_| BlockExpr::parse(parser).map(|v| v.map(Self::Block)))
                .or_else(|_| IfElseExpr::parse(parser).map(|v| v.map(Self::IfElse)))
                .or_else(|_| WhileExpr::parse(parser).map(|v| v.map(Self::While)))
                .or_else(|_| ArrayExpr::parse(parser).map(|v| v.map(Self::Array)))
                .or_else(|_| TypeIndexExpr::parse(parser).map(|v| v.map(Self::TypeIndex)))
                .or_else(|_| EnumPathExpr::parse(parser).map(|v| v.map(Self::EnumPath)))
                .or_else(|_| ClosureExpr::parse(parser).map(|v| v.map(Self::Closure)))
                .or_else(|_| Ident::parse(parser).map(|v| v.map(Self::Ident)))
                .or_else(|_| parser.consume(&Token::This).map(|v| v.wrap(Self::This)))
        } else {
            LiteralExpr::parse(parser)
                .map(|v| v.map(Self::Literal))
                .or_else(|_| NodeExpr::parse(parser).map(|v| v.map(Self::Node)))
                .or_else(|_| BlockExpr::parse(parser).map(|v| v.map(Self::Block)))
                .or_else(|_| IfElseExpr::parse(parser).map(|v| v.map(Self::IfElse)))
                .or_else(|_| WhileExpr::parse(parser).map(|v| v.map(Self::While)))
                .or_else(|_| ArrayExpr::parse(parser).map(|v| v.map(Self::Array)))
                .or_else(|_| TypeIndexExpr::parse(parser).map(|v| v.map(Self::TypeIndex)))
                .or_else(|_| EnumPathExpr::parse(parser).map(|v| v.map(Self::EnumPath)))
                .or_else(|_| ClosureExpr::parse(parser).map(|v| v.map(Self::Closure)))
                .or_else(|_| Ident::parse(parser).map(|v| v.map(Self::Ident)))
                .or_else(|_| parser.consume(&Token::This).map(|v| v.wrap(Self::This)))
        }
    }

    fn parse_pratt_expr(parser: &mut Parser, precedence: Precedence, is_limited_expr: bool) -> ParseResult<Positioned<Self>> {
        let left = Self::parse_atom(parser, is_limited_expr)?;

        go_parse_pratt_expr(parser, precedence, left)
    }
}

impl Parse for Expr {
    fn parse(parser: &mut Parser) -> ParseResult<Positioned<Self>> {
        Self::parse_pratt_expr(parser, Precedence::PLowest, false)
    }
}
