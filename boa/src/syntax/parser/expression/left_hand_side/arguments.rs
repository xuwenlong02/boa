//! Argument parsing.
//!
//! More information:
//!  - [MDN documentation][mdn]
//!  - [ECMAScript specification][spec]
//!
//! [mdn]: https://developer.mozilla.org/en-US/docs/Glossary/Argument
//! [spec]: https://tc39.es/ecma262/#prod-Arguments

use crate::{
    syntax::{
        ast::{node::Node, punc::Punctuator, token::TokenKind},
        parser::{
            expression::AssignmentExpression, AllowAwait, AllowYield, Cursor, ParseError,
            TokenParser,
        },
    },
    Interner,
};

/// Parses a list of arguments.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Glossary/Argument
/// [spec]: https://tc39.es/ecma262/#prod-Arguments
#[derive(Debug, Clone, Copy)]
pub(in crate::syntax::parser::expression) struct Arguments {
    allow_yield: AllowYield,
    allow_await: AllowAwait,
}

impl Arguments {
    /// Creates a new `Arguments` parser.
    pub(in crate::syntax::parser::expression) fn new<Y, A>(allow_yield: Y, allow_await: A) -> Self
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

impl TokenParser for Arguments {
    type Output = Vec<Node>;

    fn parse(
        self,
        cursor: &mut Cursor<'_>,
        interner: &mut Interner,
    ) -> Result<Vec<Node>, ParseError> {
        cursor.expect(Punctuator::OpenParen, "arguments", interner)?;
        let mut args = Vec::new();
        loop {
            let next_token = cursor.next().ok_or(ParseError::AbruptEnd)?;
            match next_token.kind {
                TokenKind::Punctuator(Punctuator::CloseParen) => break,
                TokenKind::Punctuator(Punctuator::Comma) => {
                    if args.is_empty() {
                        return Err(ParseError::unexpected(
                            next_token.display(interner).to_string(),
                            next_token.pos,
                            None,
                        ));
                    }

                    if cursor.next_if(Punctuator::CloseParen).is_some() {
                        break;
                    }
                }
                _ => {
                    if !args.is_empty() {
                        return Err(ParseError::expected(
                            vec![
                                Punctuator::Comma.to_string(),
                                Punctuator::CloseParen.to_string(),
                            ],
                            next_token.display(interner).to_string(),
                            next_token.pos,
                            "argument list",
                        ));
                    } else {
                        cursor.back();
                    }
                }
            }

            if cursor.next_if(Punctuator::Spread).is_some() {
                args.push(Node::spread(
                    AssignmentExpression::new(true, self.allow_yield, self.allow_await)
                        .parse(cursor, interner)?,
                ));
            } else {
                args.push(
                    AssignmentExpression::new(true, self.allow_yield, self.allow_await)
                        .parse(cursor, interner)?,
                );
            }
        }
        Ok(args)
    }
}
