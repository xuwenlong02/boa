use super::FormalParameters;
use crate::{
    syntax::{
        ast::{keyword::Keyword, node::Node, punc::Punctuator, token::TokenKind},
        parser::{
            AllowAwait, AllowDefault, AllowYield, Cursor, ParseError, ParseResult, StatementList,
            TokenParser,
        },
    },
    Interner,
};

/// Function declaration parsing
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/function
/// [spec]: https://tc39.es/ecma262/#prod-FunctionDeclaration
#[derive(Debug, Clone, Copy)]
pub(in crate::syntax::parser) struct FunctionDeclaration {
    allow_yield: AllowYield,
    allow_await: AllowAwait,
    allow_default: AllowDefault,
}

impl FunctionDeclaration {
    /// Creates a new `FunctionDeclaration` parser.
    pub(in crate::syntax::parser) fn new<Y, A, D>(
        allow_yield: Y,
        allow_await: A,
        allow_default: D,
    ) -> Self
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

        let token = cursor
            .next_skip_lineterminator()
            .ok_or(ParseError::AbruptEnd)?;
        let name = if let TokenKind::Identifier(name) = &token.kind {
            name.clone()
        } else {
            return Err(ParseError::expected(
                vec![String::from("function name")],
                token.display(interner).to_string(),
                token.pos,
                "function declaration",
            ));
        };

        cursor.expect(
            TokenKind::Punctuator(Punctuator::OpenParen),
            "function declaration",
            interner,
        )?;

        let params = FormalParameters::new(false, false).parse(cursor, interner)?;

        cursor.expect(Punctuator::OpenBlock, "function declaration", interner)?;

        let body = StatementList::new(self.allow_yield, self.allow_await, true, true)
            .parse(cursor, interner)
            .map(Node::StatementList)?;

        cursor.expect(Punctuator::CloseBlock, "function declaration", interner)?;

        Ok(Node::function_decl(name, params, body))
    }
}
