//! Array initializer parsing.
//!
//! More information:
//!  - [MDN documentation][mdn]
//!  - [ECMAScript specification][spec]
//!
//! [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array
//! [spec]: https://tc39.es/ecma262/#sec-array-initializer

use super::AssignmentExpression;
use crate::syntax::{
    ast::{constant::Const, node::Node, punc::Punctuator, token::TokenKind},
    parser::{Cursor, ParseError, ParseResult, TokenParser},
};

/// Parses an array literal.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array
/// [spec]: https://tc39.es/ecma262/#prod-ArrayLiteral
#[derive(Debug, Clone, Copy)]
pub(super) struct ArrayLiteral;

impl TokenParser for ArrayLiteral {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        let mut elements = Vec::new();

        loop {
            // TODO: Support all features.
            while cursor
                .next_if(TokenKind::Punctuator(Punctuator::Comma))
                .is_some()
            {
                elements.push(Node::Const(Const::Undefined));
            }

            if cursor
                .next_if(TokenKind::Punctuator(Punctuator::CloseBracket))
                .is_some()
            {
                break;
            }

            let _ = cursor.peek(0).ok_or(ParseError::AbruptEnd)?; // Check that there are more tokens to read.

            if cursor
                .next_if_skip_lineterminator(TokenKind::Punctuator(Punctuator::Spread))
                .is_some()
            {
                let node = AssignmentExpression::parse(cursor)?;
                elements.push(Node::spread(node));
            } else {
                elements.push(AssignmentExpression::parse(cursor)?);
            }
            cursor.next_if(TokenKind::Punctuator(Punctuator::Comma));
        }

        Ok(Node::ArrayDecl(elements))
    }
}
