//! Arrow function parsing.
//!
//! More information:
//!  - [MDN documentation][mdn]
//!  - [ECMAScript specification][spec]
//!
//! [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/Arrow_functions
//! [spec]: https://tc39.es/ecma262/#sec-arrow-function-definitions

use super::read_formal_parameters;
use crate::syntax::{
    ast::{
        node::{FormalParameter, Node},
        punc::Punctuator,
        token::TokenKind,
    },
    parser::{read_statements, AssignmentExpression, Cursor, ParseError, ParseResult, TokenParser},
};

/// Arrow function parsing.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/Arrow_functions
/// [spec]: https://tc39.es/ecma262/#prod-ArrowFunction
#[derive(Debug, Clone, Copy)]
pub(in crate::syntax::parser) struct ArrowFunction;

impl TokenParser for ArrowFunction {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        let next_token = cursor.next().ok_or(ParseError::AbruptEnd)?;
        let params = match &next_token.kind {
            TokenKind::Punctuator(Punctuator::OpenParen) => read_formal_parameters(cursor)?,
            TokenKind::Identifier(param_name) => vec![FormalParameter {
                init: None,
                name: param_name.clone(),
                is_rest_param: false,
            }],
            _ => {
                return Err(ParseError::Expected(
                    vec![
                        TokenKind::Punctuator(Punctuator::OpenParen),
                        TokenKind::identifier("identifier"),
                    ],
                    next_token.clone(),
                    "arrow function",
                ))
            }
        };

        cursor.expect_punc(Punctuator::Arrow, "arrow function")?;

        cursor.skip(|tk| tk.kind == TokenKind::LineTerminator);
        let body = match cursor.peek(0) {
            Some(tk) if tk.kind == TokenKind::Punctuator(Punctuator::OpenBlock) => {
                let _ = cursor.next();
                let body = read_statements(cursor, true).map(Node::StatementList)?;
                cursor.expect_punc(Punctuator::CloseBlock, "arrow function")?;
                body
            }
            _ => Node::return_node(AssignmentExpression::parse(cursor)?),
        };

        Ok(Node::arrow_function_decl(params, body))
    }
}
