//! Exponentiation operator parsing.
//!
//! More information:
//!  - [MDN documentation][mdn]
//!  - [ECMAScript specification][spec]
//!
//! [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Arithmetic_Operators#Exponentiation
//! [spec]: https://tc39.es/ecma262/#sec-exp-operator

use super::{unary_operator::UnaryExpression, update_expression::UpdateExpression};
use crate::syntax::{
    ast::{
        keyword::Keyword,
        node::Node,
        op::{BinOp, NumOp},
        punc::Punctuator,
        token::TokenKind,
    },
    parser::{Cursor, ParseResult, TokenParser},
};

/// Parses an exponentiation expression.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Arithmetic_Operators#Exponentiation
/// [spec]: https://tc39.es/ecma262/#prod-ExponentiationExpression
#[derive(Debug, Clone, Copy)]
pub(super) struct ExponentiationExpression;

impl ExponentiationExpression {
    /// Checks by looking at the next token to see whether it's a unary operator or not.
    fn is_unary_expression(cursor: &mut Cursor<'_>) -> bool {
        if let Some(tok) = cursor.peek(0) {
            match tok.kind {
                TokenKind::Keyword(Keyword::Delete)
                | TokenKind::Keyword(Keyword::Void)
                | TokenKind::Keyword(Keyword::TypeOf)
                | TokenKind::Punctuator(Punctuator::Add)
                | TokenKind::Punctuator(Punctuator::Sub)
                | TokenKind::Punctuator(Punctuator::Not)
                | TokenKind::Punctuator(Punctuator::Neg) => true,
                _ => false,
            }
        } else {
            false
        }
    }
}

impl TokenParser for ExponentiationExpression {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        if Self::is_unary_expression(cursor) {
            return UnaryExpression::parse(cursor);
        }

        let lhs = UpdateExpression::parse(cursor)?;
        if let Some(tok) = cursor.next() {
            if let TokenKind::Punctuator(Punctuator::Exp) = tok.kind {
                return Ok(Node::bin_op(
                    BinOp::Num(NumOp::Exp),
                    lhs,
                    Self::parse(cursor)?,
                ));
            } else {
                cursor.back();
            }
        }
        Ok(lhs)
    }
}
