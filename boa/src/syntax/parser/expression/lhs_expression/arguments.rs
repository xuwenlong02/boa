//! Argument parsing.
//!
//! More information:
//!  - [MDN documentation][mdn]
//!  - [ECMAScript specification][spec]
//!
//! [mdn]: https://developer.mozilla.org/en-US/docs/Glossary/Argument
//! [spec]: https://tc39.es/ecma262/#prod-Arguments

use crate::syntax::{
    ast::{node::Node, punc::Punctuator, token::TokenKind},
    parser::{
        expression::assignment_operator::AssignmentExpression, Cursor, ParseError, TokenParser,
    },
};

/// Parses a list of arguments.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Glossary/Argument
/// [spec]: https://tc39.es/ecma262/#prod-Arguments
pub(in crate::syntax::parser::expression) fn read_arguments(
    cursor: &mut Cursor<'_>,
) -> Result<Vec<Node>, ParseError> {
    let mut args = Vec::new();
    loop {
        let next_token = cursor
            .next_skip_lineterminator()
            .ok_or(ParseError::AbruptEnd)?;
        match next_token.kind {
            TokenKind::Punctuator(Punctuator::CloseParen) => break,
            TokenKind::Punctuator(Punctuator::Comma) => {
                if args.is_empty() {
                    return Err(ParseError::Unexpected(next_token.clone(), None));
                }

                if cursor
                    .next_if_skip_lineterminator(TokenKind::Punctuator(Punctuator::CloseParen))
                    .is_some()
                {
                    break;
                }
            }
            _ => {
                if !args.is_empty() {
                    return Err(ParseError::Expected(
                        vec![
                            TokenKind::Punctuator(Punctuator::Comma),
                            TokenKind::Punctuator(Punctuator::CloseParen),
                        ],
                        next_token.clone(),
                        "argument list",
                    ));
                } else {
                    cursor.back();
                }
            }
        }

        if cursor
            .next_if(TokenKind::Punctuator(Punctuator::Spread))
            .is_some()
        {
            args.push(Node::spread(AssignmentExpression::parse(cursor)?));
        } else {
            args.push(AssignmentExpression::parse(cursor)?);
        }
    }
    Ok(args)
}
