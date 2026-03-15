mod array;
mod as_expr;
mod binary;
mod block;
mod call;
mod closure_expr;
mod for_in_expr;
mod ident;
mod if_else;
mod index;
mod literal;
mod loop_expr;
mod node;
mod type_index;
mod while_expr;

use mollie_lexer::Token;
use mollie_shared::{Operator, Positioned};
use mollie_typing::PrimitiveType;

pub use self::{
    array::ArrayExpr,
    as_expr::{IsExpr, IsPattern, NameValuePattern, TypePattern},
    binary::BinaryExpr,
    block::{BlockExpr, parse_statements_until},
    call::FuncCallExpr,
    closure_expr::ClosureExpr,
    for_in_expr::ForInExpr,
    ident::Ident,
    if_else::IfElseExpr,
    index::{IndexExpr, IndexTarget},
    literal::{LiteralExpr, Number, SizeType},
    node::{NameValue, NodeExpr},
    type_index::{TypePathExpr, TypePathSegment},
    while_expr::WhileExpr,
};
use crate::{Parse, ParseError, ParseResult, Parser};

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
pub enum Precedence {
    PLowest,
    PAssign,
    POr,
    PAnd,
    Cmp,
    PBitOr,
    PBitAnd,
    PSum,
    PProduct,
    PCall,
    PIndex,
    PCheck,
    PCast,
}

impl Precedence {
    const fn from_ref(token: &Token) -> (Self, Option<Operator>) {
        match &token {
            Token::AndAnd => (Self::PAnd, Some(Operator::And)),
            Token::OrOr => (Self::POr, Some(Operator::Or)),
            Token::And => (Self::PBitAnd, Some(Operator::BitAnd)),
            Token::AndEq => (Self::PAssign, Some(Operator::BitAndAssign)),
            Token::Or => (Self::PBitOr, Some(Operator::BitOr)),
            Token::OrEq => (Self::PAssign, Some(Operator::BitOrAssign)),
            Token::Eq => (Self::PAssign, Some(Operator::Assign)),
            Token::EqEq => (Self::Cmp, Some(Operator::Equal)),
            Token::NotEq => (Self::Cmp, Some(Operator::NotEqual)),
            Token::Less => (Self::Cmp, Some(Operator::LessThan)),
            Token::LessEq => (Self::Cmp, Some(Operator::LessThanEqual)),
            Token::Greater => (Self::Cmp, Some(Operator::GreaterThan)),
            Token::GreaterEq => (Self::Cmp, Some(Operator::GreaterThanEqual)),
            Token::Plus => (Self::PSum, Some(Operator::Add)),
            Token::PlusEq => (Self::PAssign, Some(Operator::AddAssign)),
            Token::Minus => (Self::PSum, Some(Operator::Sub)),
            Token::MinusEq => (Self::PAssign, Some(Operator::SubAssign)),
            Token::Star => (Self::PProduct, Some(Operator::Mul)),
            Token::StarEq => (Self::PAssign, Some(Operator::MulAssign)),
            Token::Slash => (Self::PProduct, Some(Operator::Div)),
            Token::SlashEq => (Self::PAssign, Some(Operator::DivAssign)),
            Token::ParenOpen => (Self::PCall, None),
            Token::BracketOpen | Token::Dot => (Self::PIndex, None),
            Token::Is => (Self::PCheck, None),
            Token::As => (Self::PCast, None),
            _ => (Self::PLowest, None),
        }
    }
}

fn go_parse_pratt_expr(parser: &mut Parser, precedence: Precedence, left: Positioned<Expr>, is_limited_expr: bool) -> ParseResult<Positioned<Expr>> {
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

                go_parse_pratt_expr(parser, precedence, left, is_limited_expr)
            }
            Precedence::PCast if precedence < Precedence::PCast => {
                parser.consume(&Token::As)?;

                let primitive = PrimitiveType::parse(parser)?;
                let left = left.between(&primitive).wrap(Expr::Cast(Box::new(left), primitive));

                go_parse_pratt_expr(parser, precedence, left, is_limited_expr)
            }
            Precedence::PCall if precedence < Precedence::PCall => {
                let left = FuncCallExpr::parse(parser, left)?.map(Expr::FunctionCall);

                go_parse_pratt_expr(parser, precedence, left, is_limited_expr)
            }
            Precedence::PIndex if precedence < Precedence::PIndex => {
                let left = IndexExpr::parse(parser, left)?.map(Expr::Index);

                go_parse_pratt_expr(parser, precedence, left, is_limited_expr)
            }
            ref peek_precedence if precedence < *peek_precedence => {
                let left = BinaryExpr::parse(parser, left, is_limited_expr)?.map(Expr::Binary);

                go_parse_pratt_expr(parser, precedence, left, is_limited_expr)
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
    TypeIndex(TypePathExpr),
    Array(ArrayExpr),
    IfElse(IfElseExpr),
    While(WhileExpr),
    Block(BlockExpr),
    ForIn(ForInExpr),
    Is(IsExpr),
    Cast(Box<Positioned<Self>>, Positioned<PrimitiveType>),
    Closure(ClosureExpr),
    Ident(Ident),
    This,
    Nothing,
}

impl Expr {
    fn parse_atom(parser: &mut Parser, is_limited_expr: bool) -> ParseResult<Positioned<Self>> {
        if parser.check(&Token::ParenOpen) {
            let ([start, end], mut parser) = parser.split(&Token::ParenOpen, &Token::ParenClose)?;

            if parser.is_empty() {
                Ok(start.between(&end).wrap(Self::Nothing))
            } else {
                Self::parse(&mut parser)
            }
        } else if is_limited_expr {
            LiteralExpr::parse(parser)
                .map(|v| v.map(Self::Literal))
                .or_else(|_| BlockExpr::parse(parser).map(|v| v.map(Self::Block)))
                .or_else(|_| IfElseExpr::parse(parser).map(|v| v.map(Self::IfElse)))
                .or_else(|_| WhileExpr::parse(parser).map(|v| v.map(Self::While)))
                .or_else(|_| ArrayExpr::parse(parser).map(|v| v.map(Self::Array)))
                .or_else(|_| ForInExpr::parse(parser).map(|v| v.map(Self::ForIn)))
                .or_else(|_| ClosureExpr::parse(parser).map(|v| v.map(Self::Closure)))
                .or_else(|_| {
                    Ident::parse(parser)
                        .or_else(|_| parser.consume_map(|token| if matches!(token, Token::Super) { Some(Ident::new("super")) } else { None }))
                        .and_then(|name| {
                            Ok(if parser.check(&Token::PathSep) {
                                TypePathExpr::parse(name.span.wrap(TypePathSegment { name, args: None }), parser, true)?.map(Self::TypeIndex)
                            } else if name.value.0 == "super" {
                                return Err(ParseError::unexpected_token(Some(&name.span.wrap(Token::Super))));
                            } else {
                                name.map(Self::Ident)
                            })
                        })
                })
                .or_else(|_| parser.consume(&Token::This).map(|v| v.wrap(Self::This)))
        } else {
            LiteralExpr::parse(parser)
                .map(|v| v.map(Self::Literal))
                .or_else(|_| BlockExpr::parse(parser).map(|v| v.map(Self::Block)))
                .or_else(|_| IfElseExpr::parse(parser).map(|v| v.map(Self::IfElse)))
                .or_else(|_| WhileExpr::parse(parser).map(|v| v.map(Self::While)))
                .or_else(|_| ArrayExpr::parse(parser).map(|v| v.map(Self::Array)))
                .or_else(|_| ForInExpr::parse(parser).map(|v| v.map(Self::ForIn)))
                .or_else(|_| ClosureExpr::parse(parser).map(|v| v.map(Self::Closure)))
                .or_else(|_| {
                    Ident::parse(parser)
                        .or_else(|_| parser.consume_map(|token| if matches!(token, Token::Super) { Some(Ident::new("super")) } else { None }))
                        .and_then(|name| {
                            Ok(if parser.check(&Token::PathSep) {
                                let path = TypePathExpr::parse(name.span.wrap(TypePathSegment { name, args: None }), parser, true)?;

                                if parser.check_one_of(&[Token::BraceOpen, Token::From]) {
                                    NodeExpr::parse(path, parser)?.map(Self::Node)
                                } else {
                                    path.map(Self::TypeIndex)
                                }
                            } else {
                                if name.value.0 == "super" {
                                    return Err(ParseError::unexpected_token(Some(&name.span.wrap(Token::Super))));
                                }

                                if parser.check_one_of(&[Token::BraceOpen, Token::From]) {
                                    NodeExpr::parse(
                                        name.span.wrap(TypePathExpr {
                                            segments: vec![name.span.wrap(TypePathSegment { name, args: None })],
                                        }),
                                        parser,
                                    )?
                                    .map(Self::Node)
                                } else {
                                    name.map(Self::Ident)
                                }
                            })
                        })
                })
                .or_else(|_| parser.consume(&Token::This).map(|v| v.wrap(Self::This)))
        }
    }

    fn parse_pratt_expr(parser: &mut Parser, precedence: Precedence, is_limited_expr: bool) -> ParseResult<Positioned<Self>> {
        let left = Self::parse_atom(parser, is_limited_expr)?;

        go_parse_pratt_expr(parser, precedence, left, is_limited_expr)
    }
}

impl Parse for Expr {
    fn parse(parser: &mut Parser) -> ParseResult<Positioned<Self>> {
        Self::parse_pratt_expr(parser, Precedence::PLowest, false)
    }
}
