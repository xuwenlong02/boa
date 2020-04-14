//! Unary operator parsing.
//!
//! More information:
//!  - [MDN documentation][mdn]
//!  - [ECMAScript specification][spec]
//!
//! [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Expressions_and_Operators#Unary
//! [spec]: https://tc39.es/ecma262/#sec-unary-operators

use super::update_expression::UpdateExpression;
use crate::syntax::{
    ast::{keyword::Keyword, node::Node, op::UnaryOp, punc::Punctuator, token::TokenKind},
    parser::{Cursor, ParseError, ParseResult, TokenParser},
};

/// Parses a unary expression.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Expressions_and_Operators#Unary
/// [spec]: https://tc39.es/ecma262/#prod-UnaryExpression
#[derive(Debug, Clone, Copy)]
pub(super) struct UnaryExpression;

impl TokenParser for UnaryExpression {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        let tok = cursor.next().ok_or(ParseError::AbruptEnd)?;
        match tok.kind {
            TokenKind::Keyword(Keyword::Delete) => {
                Ok(Node::unary_op(UnaryOp::Delete, Self::parse(cursor)?))
            }
            TokenKind::Keyword(Keyword::Void) => {
                Ok(Node::unary_op(UnaryOp::Void, Self::parse(cursor)?))
            }
            TokenKind::Keyword(Keyword::TypeOf) => {
                Ok(Node::unary_op(UnaryOp::TypeOf, Self::parse(cursor)?))
            }
            TokenKind::Punctuator(Punctuator::Add) => {
                Ok(Node::unary_op(UnaryOp::Plus, Self::parse(cursor)?))
            }
            TokenKind::Punctuator(Punctuator::Sub) => {
                Ok(Node::unary_op(UnaryOp::Minus, Self::parse(cursor)?))
            }
            TokenKind::Punctuator(Punctuator::Neg) => {
                Ok(Node::unary_op(UnaryOp::Tilde, Self::parse(cursor)?))
            }
            TokenKind::Punctuator(Punctuator::Not) => {
                Ok(Node::unary_op(UnaryOp::Not, Self::parse(cursor)?))
            }
            _ => {
                cursor.back();
                UpdateExpression::parse(cursor)
            }
        }
    }
}
