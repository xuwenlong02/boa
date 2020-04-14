//! Conditional operator parsing.
//!
//! More information:
//!  - [MDN documentation][mdn]
//!  - [ECMAScript specification][spec]
//!
//! [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Conditional_Operator
//! [spec]: https://tc39.es/ecma262/#sec-conditional-operator

use super::{assignment_operator::AssignmentExpression, LogicalORExpression};
use crate::syntax::{
    ast::{node::Node, punc::Punctuator, token::TokenKind},
    parser::{Cursor, ParseResult, TokenParser},
};

/// Conditional expression parsing.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Conditional_Operator
/// [spec]: https://tc39.es/ecma262/#prod-ConditionalExpression
#[derive(Debug, Clone, Copy)]
pub(super) struct ConditionalExpression;

impl TokenParser for ConditionalExpression {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        // TODO: coalesce expression
        let lhs = LogicalORExpression::parse(cursor)?;

        if let Some(tok) = cursor.next() {
            if tok.kind == TokenKind::Punctuator(Punctuator::Question) {
                let then_clause = AssignmentExpression::parse(cursor)?;
                cursor.expect_punc(Punctuator::Colon, "conditional expression")?;

                let else_clause = AssignmentExpression::parse(cursor)?;
                return Ok(Node::conditional_op(lhs, then_clause, else_clause));
            } else {
                cursor.back();
            }
        }

        Ok(lhs)
    }
}
