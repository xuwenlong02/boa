//! Member expression parsing.
//!
//! More information:
//!  - [ECMAScript specification][spec]
//!
//! [spec]: https://tc39.es/ecma262/#prod-MemberExpression

use super::arguments::read_arguments;
use crate::syntax::{
    ast::{keyword::Keyword, node::Node, punc::Punctuator, token::TokenKind},
    parser::{
        expression::{primary_expression::PrimaryExpression, Expression},
        Cursor, ParseError, ParseResult, TokenParser,
    },
};

/// Parses a member expression.
///
/// More information:
///  - [ECMAScript specification][spec]
///
/// [spec]: https://tc39.es/ecma262/#prod-MemberExpression
#[derive(Debug, Clone, Copy)]
pub(super) struct MemberExpression;

impl TokenParser for MemberExpression {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        let mut lhs = if cursor
            .peek_skip_lineterminator()
            .ok_or(ParseError::AbruptEnd)?
            .kind
            == TokenKind::Keyword(Keyword::New)
        {
            let _ = cursor
                .next_skip_lineterminator()
                .expect("keyword disappeared");
            let lhs = Self::parse(cursor)?;
            cursor.expect_punc(Punctuator::OpenParen, "member expression")?;
            let args = read_arguments(cursor)?;
            let call_node = Node::call(lhs, args);

            Node::new(call_node)
        } else {
            PrimaryExpression::parse(cursor)?
        };
        while let Some(tok) = cursor.peek_skip_lineterminator() {
            match &tok.kind {
                TokenKind::Punctuator(Punctuator::Dot) => {
                    let _ = cursor
                        .next_skip_lineterminator()
                        .ok_or(ParseError::AbruptEnd)?; // We move the cursor forward.
                    match &cursor
                        .next_skip_lineterminator()
                        .ok_or(ParseError::AbruptEnd)?
                        .kind
                    {
                        TokenKind::Identifier(name) => lhs = Node::get_const_field(lhs, name),
                        TokenKind::Keyword(kw) => lhs = Node::get_const_field(lhs, kw.to_string()),
                        _ => {
                            return Err(ParseError::Expected(
                                vec![TokenKind::identifier("identifier")],
                                tok.clone(),
                                "member expression",
                            ));
                        }
                    }
                }
                TokenKind::Punctuator(Punctuator::OpenBracket) => {
                    let _ = cursor
                        .next_skip_lineterminator()
                        .ok_or(ParseError::AbruptEnd)?; // We move the cursor forward.
                    let idx = Expression::parse(cursor)?;
                    cursor.expect_punc(Punctuator::CloseBracket, "member expression")?;
                    lhs = Node::get_field(lhs, idx);
                }
                _ => break,
            }
        }

        Ok(lhs)
    }
}
