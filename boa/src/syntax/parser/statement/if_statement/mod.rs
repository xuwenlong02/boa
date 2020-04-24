#[cfg(test)]
mod tests;

use super::Statement;
use crate::{
    syntax::{
        ast::{keyword::Keyword, node::Node, punc::Punctuator, token::TokenKind},
        parser::{
            AllowAwait, AllowReturn, AllowYield, Cursor, Expression, ParseResult, TokenParser,
        },
    },
    Interner,
};

/// If statement parsing.
///
/// An _If_ statement will have a condition, a block statemet, and an optional _else_ statement.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/if...else
/// [spec]: https://tc39.es/ecma262/#prod-IfStatement
#[derive(Debug, Clone, Copy)]
pub(super) struct IfStatement {
    allow_yield: AllowYield,
    allow_await: AllowAwait,
    allow_return: AllowReturn,
}

impl IfStatement {
    /// Creates a new `IfStatement` parser.
    pub(super) fn new<Y, A, R>(allow_yield: Y, allow_await: A, allow_return: R) -> Self
    where
        Y: Into<AllowYield>,
        A: Into<AllowAwait>,
        R: Into<AllowReturn>,
    {
        Self {
            allow_yield: allow_yield.into(),
            allow_await: allow_await.into(),
            allow_return: allow_return.into(),
        }
    }
}

impl TokenParser for IfStatement {
    type Output = Node;

    fn parse(self, cursor: &mut Cursor<'_>, interner: &mut Interner) -> ParseResult {
        cursor.expect(Keyword::If, "if statement", interner)?;
        cursor.expect(Punctuator::OpenParen, "if statement", interner)?;

        let cond =
            Expression::new(true, self.allow_yield, self.allow_await).parse(cursor, interner)?;

        cursor.expect(Punctuator::CloseParen, "if statement", interner)?;

        let then_stm = Statement::new(self.allow_yield, self.allow_await, self.allow_return)
            .parse(cursor, interner)?;

        let else_stm = match cursor.next() {
            Some(else_tok) if else_tok.kind == TokenKind::Keyword(Keyword::Else) => Some(
                Statement::new(self.allow_yield, self.allow_await, self.allow_return)
                    .parse(cursor, interner)?,
            ),
            _ => {
                cursor.back();
                None
            }
        };

        Ok(Node::if_node::<_, _, Node, _>(cond, then_stm, else_stm))
    }
}
