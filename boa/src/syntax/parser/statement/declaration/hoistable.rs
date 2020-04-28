//! Hoistable declaration parsing.
//!
//! More information:
//!  - [ECMAScript specification][spec]
//!
//! [spec]: https://tc39.es/ecma262/#prod-HoistableDeclaration

use crate::{
    syntax::{
        ast::{keyword::Keyword, node::Node, punc::Punctuator, token::TokenKind},
        parser::{
            function::FormalParameters, function::FunctionBody, AllowAwait, AllowDefault,
            AllowYield, Cursor, ParseError, ParseResult, TokenParser,
        },
    },
    Interner,
};

/// Hoistable declaration parsing.
///
/// More information:
///  - [ECMAScript specification][spec]
///
/// [spec]: https://tc39.es/ecma262/#prod-FunctionDeclaration
#[derive(Debug, Clone, Copy)]
pub(super) struct HoistableDeclaration {
    allow_yield: AllowYield,
    allow_await: AllowAwait,
    allow_default: AllowDefault,
}

impl HoistableDeclaration {
    /// Creates a new `HoistableDeclaration` parser.
    pub(super) fn new<Y, A, D>(allow_yield: Y, allow_await: A, allow_default: D) -> Self
    where
        Y: Into<AllowYield>,
        A: Into<AllowAwait>,
        D: Into<AllowDefault>,
    {
        Self {
            allow_yield: allow_yield.into(),
            allow_await: allow_await.into(),
            allow_default: allow_default.into(),
        }
    }
}

impl TokenParser for HoistableDeclaration {
    type Output = Node;

    fn parse(self, cursor: &mut Cursor<'_>, interner: &mut Interner) -> ParseResult {
        // TODO: check for generators and async functions + generators
        FunctionDeclaration::new(self.allow_yield, self.allow_await, self.allow_default)
            .parse(cursor, interner)
    }
}

/// Function declaration parsing.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/function
/// [spec]: https://tc39.es/ecma262/#prod-FunctionDeclaration
#[derive(Debug, Clone, Copy)]
struct FunctionDeclaration {
    allow_yield: AllowYield,
    allow_await: AllowAwait,
    allow_default: AllowDefault,
}

impl FunctionDeclaration {
    /// Creates a new `FunctionDeclaration` parser.
    fn new<Y, A, D>(allow_yield: Y, allow_await: A, allow_default: D) -> Self
    where
        Y: Into<AllowYield>,
        A: Into<AllowAwait>,
        D: Into<AllowDefault>,
    {
        Self {
            allow_yield: allow_yield.into(),
            allow_await: allow_await.into(),
            allow_default: allow_default.into(),
        }
    }
}

impl TokenParser for FunctionDeclaration {
    type Output = Node;

    fn parse(self, cursor: &mut Cursor<'_>, interner: &mut Interner) -> ParseResult {
        cursor.expect(Keyword::Function, "function declaration", interner)?;

        let token = cursor.next().ok_or(ParseError::AbruptEnd)?;
        let name = if let TokenKind::Identifier(name) = token.kind {
            name.clone()
        } else {
            return Err(ParseError::Expected(
                vec![TokenKind::identifier("function name")],
                token.clone(),
                "function declaration",
            ));
        };

        cursor.expect(Punctuator::OpenParen, "function declaration", interner)?;

        let params = FormalParameters::new(false, false).parse(cursor, interner)?;

        cursor.expect(Punctuator::CloseParen, "function declaration", interner)?;
        cursor.expect(Punctuator::OpenBlock, "function declaration", interner)?;

        let body = FunctionBody::new(self.allow_yield, self.allow_await)
            .parse(cursor, interner)
            .map(Node::StatementList)?;

        cursor.expect(Punctuator::CloseBlock, "function declaration", interner)?;

        Ok(Node::function_decl(name, params, body))
    }
}
