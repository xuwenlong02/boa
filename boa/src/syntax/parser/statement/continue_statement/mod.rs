#[cfg(test)]
mod tests;

use crate::{
    syntax::{
        ast::{keyword::Keyword, node::Node, punc::Punctuator, token::TokenKind},
        parser::{AllowAwait, AllowYield, Cursor, ParseError, ParseResult, TokenParser},
    },
    Interner,
};

/// For statement parsing
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/continue
/// [spec]: https://tc39.es/ecma262/#sec-continue-statement
#[derive(Debug, Clone, Copy)]
pub(super) struct ContinueStatement {
    allow_yield: AllowYield,
    allow_await: AllowAwait,
}

impl ContinueStatement {
    /// Creates a new `ContinueStatement` parser.
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

impl TokenParser for ContinueStatement {
    type Output = Node;

    fn parse(self, cursor: &mut Cursor<'_>, interner: &mut Interner) -> ParseResult {
        cursor.expect(Keyword::Continue, "continue statement", interner)?;

        let tok = cursor.next().ok_or(ParseError::AbruptEnd)?;
        match tok.kind {
            TokenKind::LineTerminator
            | TokenKind::Punctuator(Punctuator::Semicolon)
            | TokenKind::Punctuator(Punctuator::CloseBlock) => {
                cursor.back();
                Ok(Node::Continue(None))
            }
            TokenKind::Identifier(name) => Ok(Node::continue_node(name)),
            _ => Err(ParseError::expected(
                vec![
                    Punctuator::Semicolon.to_string(),
                    TokenKind::LineTerminator.display(interner).to_string(),
                    Punctuator::CloseBlock.to_string(),
                ],
                tok.display(interner).to_string(),
                tok.pos,
                "continue statement",
            )),
        }
    }
}
