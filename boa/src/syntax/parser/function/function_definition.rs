//! Function definition parsing.
//!
//! More information:
//!  - [MDN documentation][mdn]
//!  - [ECMAScript specification][spec]
//!
//! [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/function
//! [spec]: https://tc39.es/ecma262/#sec-function-definitions

use super::read_formal_parameters;
use crate::syntax::{
    ast::{node::Node, punc::Punctuator, token::TokenKind},
    parser::{read_statements, Cursor, ParseError, ParseResult, TokenParser},
};

/// Function expression parsing.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/function
/// [spec]: https://tc39.es/ecma262/#prod-FunctionExpression
#[derive(Debug, Clone, Copy)]
pub(in crate::syntax::parser) struct FunctionExpression;

impl TokenParser for FunctionExpression {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        let name = if let TokenKind::Identifier(name) =
            &cursor.peek(0).ok_or(ParseError::AbruptEnd)?.kind
        {
            Some(name)
        } else {
            None
        };
        if name.is_some() {
            // We move the cursor forward.
            let _ = cursor.next().expect("nex token disappeared");
        }

        cursor.expect_punc(Punctuator::OpenParen, "function expression")?;

        let params = read_formal_parameters(cursor)?;

        cursor.expect_punc(Punctuator::OpenBlock, "function expression")?;

        let body = read_statements(cursor, true).map(Node::StatementList)?;

        cursor.expect_punc(Punctuator::CloseBlock, "function expression")?;

        Ok(Node::function_decl::<_, &String, _, _>(name, params, body))
    }
}
