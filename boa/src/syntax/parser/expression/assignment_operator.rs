//! Assignment operator parsing.
//!
//! More information:
//!  - [MDN documentation][mdn]
//!  - [ECMAScript specification][spec]
//!
//! [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Assignment_Operators#Assignment
//! [spec]: https://tc39.es/ecma262/#sec-assignment-operators

use super::conditional_operator::ConditionalExpression;
use crate::syntax::{
    ast::{node::Node, punc::Punctuator, token::TokenKind},
    parser::{ArrowFunction, Cursor, ParseError, ParseResult, TokenParser},
};

/// Assignment expression parsing.
///
/// This can be one of the following:
///
///  - `ConditionalExpression`
///  - `YieldExpression`
///  - `ArrowFunction`
///  - `AsyncArrowFunction`
///  - `LeftHandSideExpression` `=` `AssignmentExpression`
///  - `LeftHandSideExpression` `AssignmentOperator` `AssignmentExpression`
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Assignment_Operators#Assignment
/// [spec]: https://tc39.es/ecma262/#prod-AssignmentExpression
#[derive(Debug, Clone, Copy)]
pub(in crate::syntax::parser) struct AssignmentExpression;

impl TokenParser for AssignmentExpression {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        // Arrow function
        let next_token = cursor.peek(0).ok_or(ParseError::AbruptEnd)?;
        match next_token.kind {
            // a=>{}
            TokenKind::Identifier(_) => {
                if let Some(tok) = cursor.peek(1) {
                    if tok.kind == TokenKind::Punctuator(Punctuator::Arrow) {
                        return ArrowFunction::parse(cursor);
                    }
                }
            }
            // (a,b)=>{}
            TokenKind::Punctuator(Punctuator::OpenParen) => {
                if let Some(node) = ArrowFunction::try_parse(cursor) {
                    return Ok(node);
                }
            }
            _ => {}
        }

        let mut lhs = ConditionalExpression::parse(cursor)?;
        // let mut lhs = self.read_block()?;

        if let Some(tok) = cursor.next() {
            match tok.kind {
                TokenKind::Punctuator(Punctuator::Assign) => {
                    lhs = Node::assign(lhs, Self::parse(cursor)?)
                }
                TokenKind::Punctuator(p) if p.as_binop().is_some() => {
                    let expr = Self::parse(cursor)?;
                    let binop = p.as_binop().expect("binop disappeared");
                    lhs = Node::bin_op(binop, lhs, expr);
                }
                _ => {
                    cursor.back();
                }
            }
        }

        Ok(lhs)
    }
}
