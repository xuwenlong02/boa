//! Update expression parsing.
//!
//! More information:
//!  - [ECMAScript specification][spec]
//!
//! [spec]: https://tc39.es/ecma262/#sec-update-expressions

use super::lhs_expression::LeftHandSideExpression;
use crate::syntax::{
    ast::{node::Node, op::UnaryOp, punc::Punctuator, token::TokenKind},
    parser::{Cursor, ParseError, ParseResult, TokenParser},
};

/// Parses an update expression.
///
/// More information:
///  - [ECMAScript specification][spec]
///
/// [spec]: https://tc39.es/ecma262/#prod-UpdateExpression
#[derive(Debug, Clone, Copy)]
pub(super) struct UpdateExpression;

impl TokenParser for UpdateExpression {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        let tok = cursor
            .peek_skip_lineterminator()
            .ok_or(ParseError::AbruptEnd)?;
        match tok.kind {
            TokenKind::Punctuator(Punctuator::Inc) => {
                cursor
                    .next_skip_lineterminator()
                    .expect("token disappeared");
                return Ok(Node::unary_op(
                    UnaryOp::IncrementPre,
                    LeftHandSideExpression::parse(cursor)?,
                ));
            }
            TokenKind::Punctuator(Punctuator::Dec) => {
                cursor
                    .next_skip_lineterminator()
                    .expect("token disappeared");
                return Ok(Node::unary_op(
                    UnaryOp::DecrementPre,
                    LeftHandSideExpression::parse(cursor)?,
                ));
            }
            _ => {}
        }

        let lhs = LeftHandSideExpression::parse(cursor)?;
        if let Some(tok) = cursor.peek(0) {
            match tok.kind {
                TokenKind::Punctuator(Punctuator::Inc) => {
                    cursor.next().expect("token disappeared");
                    return Ok(Node::unary_op(UnaryOp::IncrementPost, lhs));
                }
                TokenKind::Punctuator(Punctuator::Dec) => {
                    cursor.next().expect("token disappeared");
                    return Ok(Node::unary_op(UnaryOp::DecrementPost, lhs));
                }
                _ => {}
            }
        }

        Ok(lhs)
    }
}
