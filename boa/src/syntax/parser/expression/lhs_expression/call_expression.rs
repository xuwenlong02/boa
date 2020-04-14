use super::arguments::read_arguments;
use crate::syntax::{
    ast::{node::Node, punc::Punctuator, token::TokenKind},
    parser::{expression::Expression, Cursor, ParseError, ParseResult, TokenParser},
};

/// Parses a call expression.
///
/// More information:
///  - [ECMAScript specification][spec]
///
/// [spec]: https://tc39.es/ecma262/#prod-CallExpression
pub(super) fn read_call_expression(
    cursor: &mut Cursor<'_>,
    first_member_expr: Node,
) -> ParseResult {
    let mut lhs = first_member_expr;
    if cursor
        .next_if_skip_lineterminator(TokenKind::Punctuator(Punctuator::OpenParen))
        .is_some()
    {
        let args = read_arguments(cursor)?;
        lhs = Node::call(lhs, args);
    } else {
        let next_token = cursor
            .next_skip_lineterminator()
            .ok_or(ParseError::AbruptEnd)?;
        return Err(ParseError::Expected(
            vec![TokenKind::Punctuator(Punctuator::OpenParen)],
            next_token.clone(),
            "call expression",
        ));
    }

    while let Some(tok) = cursor.peek_skip_lineterminator() {
        match tok.kind {
            TokenKind::Punctuator(Punctuator::OpenParen) => {
                let _ = cursor
                    .next_skip_lineterminator()
                    .ok_or(ParseError::AbruptEnd)?; // We move the cursor.
                let args = read_arguments(cursor)?;
                lhs = Node::call(lhs, args);
            }
            TokenKind::Punctuator(Punctuator::Dot) => {
                let _ = cursor
                    .next_skip_lineterminator()
                    .ok_or(ParseError::AbruptEnd)?; // We move the cursor.
                match &cursor
                    .next_skip_lineterminator()
                    .ok_or(ParseError::AbruptEnd)?
                    .kind
                {
                    TokenKind::Identifier(name) => {
                        lhs = Node::get_const_field(lhs, name);
                    }
                    TokenKind::Keyword(kw) => {
                        lhs = Node::get_const_field(lhs, kw.to_string());
                    }
                    _ => {
                        return Err(ParseError::Expected(
                            vec![TokenKind::identifier("identifier")],
                            tok.clone(),
                            "call expression",
                        ));
                    }
                }
            }
            TokenKind::Punctuator(Punctuator::OpenBracket) => {
                let _ = cursor
                    .next_skip_lineterminator()
                    .ok_or(ParseError::AbruptEnd)?; // We move the cursor.
                let idx = Expression::parse(cursor)?;
                cursor.expect_punc(Punctuator::CloseBracket, "call expression")?;
                lhs = Node::get_field(lhs, idx);
            }
            _ => break,
        }
    }
    Ok(lhs)
}
