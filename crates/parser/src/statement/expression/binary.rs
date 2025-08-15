// use std::fmt;

use mollie_lexer::Token;
use mollie_shared::{Operator, Positioned};

use crate::{Expr, ParseError, ParseResult, Parser, Precedence};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct BinaryExpr {
    pub lhs: Box<Positioned<Expr>>,
    pub rhs: Box<Positioned<Expr>>,
    pub operator: Positioned<Operator>,
}

// impl fmt::Display for BinaryExpression {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         write!(f, "{} {} {}", self.lhs, self.operator, self.rhs)
//     }
// }

impl BinaryExpr {
    /// # Errors
    ///
    /// Returns `ParseError` if parsing failed
    pub fn parse(parser: &mut Parser, lhs: Positioned<Expr>) -> ParseResult<Positioned<Self>> {
        let peeked = parser.peek().ok_or_else(|| ParseError::new("expected operator", None))?;

        let (precedence, operator) = Precedence::from_ref(&peeked.value);

        let operator = peeked
            .span
            .wrap(operator.ok_or_else(|| ParseError::expected_tokens(&[Token::Plus, Token::Minus, Token::Star, Token::Slash], Some(peeked)))?);

        parser.next();

        let rhs = Expr::parse_pratt_expr(parser, precedence)?;

        Ok(lhs.between(&rhs).wrap(Self {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            operator,
        }))
    }
}
