//! Object initializer parsing.
//!
//! More information:
//!  - [MDN documentation][mdn]
//!  - [ECMAScript specification][spec]
//!
//! [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Object_initializer
//! [spec]: https://tc39.es/ecma262/#sec-object-initializer

#[cfg(test)]
mod tests;

use super::assignment_operator::AssignmentExpression;
use crate::{
    syntax::{
        ast::{
            node::{self, MethodDefinitionKind, Node},
            punc::Punctuator,
            token::TokenKind,
        },
        parser::{
            AllowAwait, AllowIn, AllowYield, Cursor, FormalParameters, FunctionBody, ParseError,
            ParseResult, TokenParser,
        },
    },
    Interner, InternerSym,
};

/// Parses an object literal.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Object_initializer
/// [spec]: https://tc39.es/ecma262/#prod-ObjectLiteral
#[derive(Debug, Clone, Copy)]
pub(super) struct ObjectLiteral {
    allow_yield: AllowYield,
    allow_await: AllowAwait,
}

impl ObjectLiteral {
    /// Creates a new `ObjectLiteral` parser.
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

impl TokenParser for ObjectLiteral {
    type Output = Node;

    fn parse(self, cursor: &mut Cursor<'_>, interner: &mut Interner) -> ParseResult {
        let mut elements = Vec::new();

        loop {
            if cursor
                .next_if_skip_lineterminator(TokenKind::Punctuator(Punctuator::CloseBlock))
                .is_some()
            {
                break;
            }

            elements.push(
                PropertyDefinition::new(self.allow_yield, self.allow_await)
                    .parse(cursor, interner)?,
            );

            if cursor
                .next_if_skip_lineterminator(TokenKind::Punctuator(Punctuator::CloseBlock))
                .is_some()
            {
                break;
            }

            if cursor
                .next_if_skip_lineterminator(TokenKind::Punctuator(Punctuator::Comma))
                .is_none()
            {
                let next_token = cursor
                    .next_skip_lineterminator()
                    .ok_or(ParseError::AbruptEnd)?;
                return Err(ParseError::expected(
                    vec![
                        Punctuator::Comma.to_string(),
                        Punctuator::CloseBlock.to_string(),
                    ],
                    next_token.display(interner).to_string(),
                    next_token.pos,
                    "object literal",
                ));
            }
        }

        Ok(Node::Object(elements))
    }
}

impl ObjectLiteral {}

/// Parses a property definition.
///
/// More information:
///  - [ECMAScript specification][spec]
///
/// [spec]: https://tc39.es/ecma262/#prod-PropertyDefinition
#[derive(Debug, Clone, Copy)]
struct PropertyDefinition {
    allow_yield: AllowYield,
    allow_await: AllowAwait,
}

impl PropertyDefinition {
    /// Creates a new `PropertyDefinition` parser.
    fn new<Y, A>(allow_yield: Y, allow_await: A) -> Self
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

impl TokenParser for PropertyDefinition {
    type Output = node::PropertyDefinition;

    fn parse(
        self,
        cursor: &mut Cursor<'_>,
        interner: &mut Interner,
    ) -> Result<Self::Output, ParseError> {
        fn to_string(kind: TokenKind, interner: &mut Interner) -> InternerSym {
            match kind {
                TokenKind::Identifier(name) => name,
                TokenKind::NumericLiteral(n) => interner.get_or_intern(format!("{}", n)),
                TokenKind::StringLiteral(s) => s,
                _ => unimplemented!("{:?}", kind),
            }
        }

        if cursor
            .next_if_skip_lineterminator(TokenKind::Punctuator(Punctuator::Spread))
            .is_some()
        {
            let node = AssignmentExpression::new(true, self.allow_yield, self.allow_await)
                .parse(cursor, interner)?;
            return Ok(node::PropertyDefinition::SpreadObject(node));
        }

        let prop_name = cursor
            .next_skip_lineterminator()
            .map(|tok| to_string(tok.kind, interner))
            .ok_or(ParseError::AbruptEnd)?;

        if cursor
            .next_if_skip_lineterminator(TokenKind::Punctuator(Punctuator::Colon))
            .is_some()
        {
            let val = AssignmentExpression::new(true, self.allow_yield, self.allow_await)
                .parse(cursor, interner)?;
            return Ok(node::PropertyDefinition::Property(prop_name, val));
        }

        // TODO: Split into separate function: read_property_method_definition
        if cursor
            .next_if_skip_lineterminator(TokenKind::Punctuator(Punctuator::OpenParen))
            .is_some()
        {
            let params = FormalParameters::new(self.allow_yield, self.allow_await)
                .parse(cursor, interner)?;

            cursor.expect(Punctuator::OpenBlock, "method definition", interner)?;

            let body = FunctionBody::new(self.allow_yield, self.allow_await)
                .parse(cursor, interner)
                .map(Node::StatementList)?;

            cursor.expect(Punctuator::CloseBlock, "method definition", interner)?;

            return Ok(node::PropertyDefinition::MethodDefinition(
                MethodDefinitionKind::Ordinary,
                prop_name,
                Node::FunctionDecl(None, params, Box::new(body)),
            ));
        }

        // TODO need to revisit this
        // if let TokenKind::Identifier(name) = tok.kind {
        //     if name == "get" || name == "set" {
        //         let may_identifier = self.peek_skip_lineterminator();
        //         if may_identifier.is_some()
        //             && matches!(may_identifier.unwrap().kind, TokenKind::Identifier(_))
        //         {
        //             let f = self.read_function_expression()?;
        //             let func_name = if let NodeBase::FunctionExpr(ref name, _, _) = f.base {
        //                 name.clone().unwrap()
        //             } else {
        //                 panic!()
        //             };
        //             return Ok(PropertyDefinition::MethodDefinition(
        //                 if name == "get" {
        //                     MethodDefinitionKind::Get
        //                 } else {
        //                     MethodDefinitionKind::Set
        //                 },
        //                 func_name,
        //                 f,
        //             ));
        //         }
        //     }

        //     return Ok(PropertyDefinition::IdentifierReference(name));
        // }

        let pos = cursor
            .peek(0)
            .map(|tok| tok.pos)
            .ok_or(ParseError::AbruptEnd)?;
        Err(ParseError::general("expected property definition", pos))
    }
}

/// Initializer parsing.
///
/// More information:
///  - [ECMAScript specification][spec]
///
/// [spec]: https://tc39.es/ecma262/#prod-Initializer
#[derive(Debug, Clone, Copy)]
pub(in crate::syntax::parser) struct Initializer {
    allow_in: AllowIn,
    allow_yield: AllowYield,
    allow_await: AllowAwait,
}

impl Initializer {
    /// Creates a new `Initializer` parser.
    pub(in crate::syntax::parser) fn new<I, Y, A>(
        allow_in: I,
        allow_yield: Y,
        allow_await: A,
    ) -> Self
    where
        I: Into<AllowIn>,
        Y: Into<AllowYield>,
        A: Into<AllowAwait>,
    {
        Self {
            allow_in: allow_in.into(),
            allow_yield: allow_yield.into(),
            allow_await: allow_await.into(),
        }
    }
}

impl TokenParser for Initializer {
    type Output = Node;

    fn parse(self, cursor: &mut Cursor<'_>, interner: &mut Interner) -> ParseResult {
        cursor.expect(Punctuator::Assign, "initializer", interner)?;
        AssignmentExpression::new(self.allow_in, self.allow_yield, self.allow_await)
            .parse(cursor, interner)
    }
}
