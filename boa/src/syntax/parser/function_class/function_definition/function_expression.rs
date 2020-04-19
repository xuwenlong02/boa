use super::{FormalParameters, FunctionBody};
use crate::syntax::{
    ast::{node::Node, punc::Punctuator, token::TokenKind},
    parser::{AllowAwait, AllowYield, Cursor, ParseError, ParseResult, TokenParser},
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
pub(in crate::syntax::parser) struct FunctionExpression {
    // This doesn't directly appear in the spec, but it seems a typo.
    // Reported in <https://github.com/tc39/ecma262/issues/1957>
    allow_yield: AllowYield,
    allow_await: AllowAwait,
}

impl FunctionExpression {
    /// Creates a new `FunctionExpression` parser.
    pub(in crate::syntax::parser) fn new<Y, A>(allow_yield: Y, allow_await: A) -> Self
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

impl TokenParser for FunctionExpression {
    type Output = Node;

    fn parse(self, cursor: &mut Cursor<'_>) -> ParseResult {
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

        cursor.expect(Punctuator::OpenParen, "function expression")?;

        let params = FormalParameters::new(false, false).parse(cursor)?;

        cursor.expect(Punctuator::OpenBlock, "function expression")?;

        let body = FunctionBody::new(false, false)
            .parse(cursor)
            .map(Node::StatementList)?;

        cursor.expect(Punctuator::CloseBlock, "function expression")?;

        Ok(Node::function_decl::<_, &String, _, _>(name, params, body))
    }
}
