use mollie_lexer::Token;
use mollie_shared::Positioned;

use crate::{Expr, IfElseExpr, Parse, ParseError, ParseResult, Parser, Stmt};

pub type BlockStmts = (Vec<Positioned<Stmt>>, Option<Positioned<Stmt>>);

/// # Errors
///
/// Returns error if parsing failed
pub fn parse_statements_until(parser: &mut Parser, token: &Token) -> ParseResult<BlockStmts> {
    let mut statements = Vec::new();
    let mut return_statement: Option<Positioned<Stmt>> = None;

    while !parser.check(token) {
        let statement = Stmt::parse(parser)?;

        if matches!(statement.value, Stmt::Expression(_))
            && !matches!(
                statement.value,
                Stmt::Expression(
                    Expr::IfElse(IfElseExpr {
                        block: Positioned {
                            value: BlockExpr { final_stmt: None, .. },
                            ..
                        },
                        ..
                    }) | Expr::Block(_)
                        | Expr::While(_)
                )
            )
        {
            if parser.try_consume(&Token::Semi) {
                statements.push(statement);
            } else if let Some(statement) = &return_statement {
                return Err(ParseError::new("missing ;", Some(statement.span)));
            } else {
                return_statement.replace(statement);
            }
        } else if return_statement.is_none() {
            statements.push(statement);
        } else {
            return Err(ParseError::new("return value already exists", Some(statement.span)));
        }
    }

    Ok((statements, return_statement))
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct BlockExpr {
    pub stmts: Vec<Positioned<Stmt>>,
    pub final_stmt: Option<Box<Positioned<Stmt>>>,
}

impl Parse for BlockExpr {
    fn parse(parser: &mut Parser) -> ParseResult<Positioned<Self>> {
        let start = parser.consume(&Token::BraceOpen)?;

        let (statements, final_statement) = parse_statements_until(parser, &Token::BraceClose)?;

        let final_statement = final_statement.map(Box::new);
        let end = parser.consume(&Token::BraceClose)?;

        Ok(start.between(&end).wrap(Self {
            stmts: statements,
            final_stmt: final_statement,
        }))
    }
}
