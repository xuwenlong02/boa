//! Left hand side expression parsing.
//!
//! More information:
//!  - [MDN documentation][mdn]
//!  - [ECMAScript specification][spec]
//!
//! [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Expressions_and_Operators#Left-hand-side_expressions
//! [spec]: https://tc39.es/ecma262/#sec-left-hand-side-expressions

mod arguments;
mod call_expression;
mod member_expression;

use self::{call_expression::read_call_expression, member_expression::MemberExpression};
use crate::syntax::{
    ast::{punc::Punctuator, token::TokenKind},
    parser::{Cursor, ParseResult, TokenParser},
};

/// Parses a left hand side expression.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Expressions_and_Operators#Left-hand-side_expressions
/// [spec]: https://tc39.es/ecma262/#prod-LeftHandSideExpression
#[derive(Debug, Clone, Copy)]
pub(super) struct LeftHandSideExpression;

impl TokenParser for LeftHandSideExpression {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        // TODO: Implement NewExpression: new MemberExpression
        let lhs = MemberExpression::parse(cursor)?;
        match cursor.peek_skip_lineterminator() {
            Some(ref tok) if tok.kind == TokenKind::Punctuator(Punctuator::OpenParen) => {
                read_call_expression(cursor, lhs)
            }
            _ => Ok(lhs), // TODO: is this correct?
        }
    }
}
