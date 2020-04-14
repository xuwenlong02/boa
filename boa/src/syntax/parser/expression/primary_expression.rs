use super::{array_initializer::ArrayLiteral, object_initializer::ObjectLiteral, Expression};
use crate::syntax::{
    ast::{constant::Const, keyword::Keyword, node::Node, punc::Punctuator, token::TokenKind},
    parser::{Cursor, FunctionExpression, ParseError, ParseResult, TokenParser},
};

/// Parses a primary expression.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Expressions_and_Operators#Primary_expressions
/// [spec]: https://tc39.es/ecma262/#prod-PrimaryExpression
#[derive(Debug, Clone, Copy)]
pub(super) struct PrimaryExpression;

impl TokenParser for PrimaryExpression {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        let tok = cursor
            .next_skip_lineterminator()
            .ok_or(ParseError::AbruptEnd)?;

        match &tok.kind {
            TokenKind::Keyword(Keyword::This) => Ok(Node::This),
            // TokenKind::Keyword(Keyword::Arguments) => Ok(Node::new(NodeBase::Arguments, tok.pos)),
            TokenKind::Keyword(Keyword::Function) => FunctionExpression::parse(cursor),
            TokenKind::Punctuator(Punctuator::OpenParen) => {
                let expr = Expression::parse(cursor)?;
                cursor.expect_punc(Punctuator::CloseParen, "primary expression")?;
                Ok(expr)
            }
            TokenKind::Punctuator(Punctuator::OpenBracket) => ArrayLiteral::parse(cursor),
            TokenKind::Punctuator(Punctuator::OpenBlock) => ObjectLiteral::parse(cursor),
            TokenKind::BooleanLiteral(boolean) => Ok(Node::const_node(*boolean)),
            // TODO: ADD TokenKind::UndefinedLiteral
            TokenKind::Identifier(ref i) if i == "undefined" => Ok(Node::Const(Const::Undefined)),
            TokenKind::NullLiteral => Ok(Node::Const(Const::Null)),
            TokenKind::Identifier(ident) => Ok(Node::local(ident)),
            TokenKind::StringLiteral(s) => Ok(Node::const_node(s)),
            TokenKind::NumericLiteral(num) => Ok(Node::const_node(*num)),
            TokenKind::RegularExpressionLiteral(body, flags) => Ok(Node::new(Node::call(
                Node::local("RegExp"),
                vec![Node::const_node(body), Node::const_node(flags)],
            ))),
            _ => Err(ParseError::Unexpected(
                tok.clone(),
                Some("primary expression"),
            )),
        }
    }
}
