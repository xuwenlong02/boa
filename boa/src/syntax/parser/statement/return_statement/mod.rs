#[cfg(test)]
mod tests;

use crate::{
    syntax::{
        ast::{keyword::Keyword, node::Node, punc::Punctuator, token::TokenKind},
        parser::{AllowAwait, AllowYield, Cursor, Expression, ParseResult, TokenParser},
    },
    Interner,
};

/// Return statement parsing
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/return
/// [spec]: https://tc39.es/ecma262/#prod-ReturnStatement
#[derive(Debug, Clone, Copy)]
pub(super) struct ReturnStatement {
    allow_yield: AllowYield,
    allow_await: AllowAwait,
}

impl ReturnStatement {
    /// Creates a new `ReturnStatement` parser.
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

impl TokenParser for ReturnStatement {
    type Output = Node;

    fn parse(self, cursor: &mut Cursor<'_>, interner: &mut Interner) -> ParseResult {
        cursor.expect(Keyword::Return, "return statement", interner)?;

        if let Some(tok) = cursor.peek(0) {
            match tok.kind {
                TokenKind::LineTerminator | TokenKind::Punctuator(Punctuator::Semicolon) => {
                    let _ = cursor.next();
                    return Ok(Node::Return(None));
                }
                TokenKind::Punctuator(Punctuator::CloseBlock) => {
                    return Ok(Node::Return(None));
                }
                _ => {}
            }
        }

        let expr =
            Expression::new(true, self.allow_yield, self.allow_await).parse(cursor, interner)?;

        cursor.expect_semicolon(false, "return statement", interner)?;

        Ok(Node::return_node(expr))
    }
}
