#[cfg(test)]
mod tests;

use crate::syntax::{
    ast::{keyword::Keyword, node::Node, punc::Punctuator, token::TokenKind},
    parser::{AllowAwait, AllowYield, Cursor, ParseError, ParseResult, TokenParser},
};

/// Break statement parsing
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/break
/// [spec]: https://tc39.es/ecma262/#sec-break-statement
#[derive(Debug, Clone, Copy)]
pub(super) struct BreakStatement {
    allow_yield: AllowYield,
    allow_await: AllowAwait,
}

impl BreakStatement {
    /// Creates a new `BreakStatement` parser.
    pub(super) fn new<Y, A>(allow_yield: Y, allow_await: A) -> Self
    where
        Y: Into<AllowYield>,
        A: Into<AllowAwait>,
    {
        Self {
            allow_yield: allow_yield.into(),
            allow_await: allow_await.into(),
        }
    }
}

impl TokenParser for BreakStatement {
    type Output = Node;

    fn parse(self, cursor: &mut Cursor<'_>) -> ParseResult {
        cursor.expect(Keyword::Break, "break statement")?;

        let tok = cursor.next().ok_or(ParseError::AbruptEnd)?;
        match &tok.kind {
            TokenKind::LineTerminator
            | TokenKind::Punctuator(Punctuator::Semicolon)
            | TokenKind::Punctuator(Punctuator::CloseBlock) => {
                cursor.back();
                Ok(Node::Break(None))
            }
            TokenKind::Identifier(name) => Ok(Node::break_node(name)),
            _ => Err(ParseError::Expected(
                vec![
                    TokenKind::Punctuator(Punctuator::Semicolon),
                    TokenKind::Punctuator(Punctuator::CloseBlock),
                    TokenKind::LineTerminator,
                    TokenKind::identifier("identifier"),
                ],
                tok.clone(),
                "break statement",
            )),
        }
    }
}
