//! Object initializer parsing.
//!
//! More information:
//!  - [MDN documentation][mdn]
//!  - [ECMAScript specification][spec]
//!
//! [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Object_initializer
//! [spec]: https://tc39.es/ecma262/#sec-object-initializer

use super::assignment_operator::AssignmentExpression;
use crate::syntax::{
    ast::{
        node::{MethodDefinitionKind, Node, PropertyDefinition},
        punc::Punctuator,
        token::TokenKind,
    },
    parser::{
        read_formal_parameters, read_statements, Cursor, ParseError, ParseResult, TokenParser,
    },
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
pub(super) struct ObjectLiteral;

impl TokenParser for ObjectLiteral {
    fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
        let mut elements = Vec::new();

        loop {
            if cursor
                .next_if_skip_lineterminator(TokenKind::Punctuator(Punctuator::CloseBlock))
                .is_some()
            {
                break;
            }

            elements.push(Self::read_property_definition(cursor)?);

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
                return Err(ParseError::Expected(
                    vec![
                        TokenKind::Punctuator(Punctuator::Comma),
                        TokenKind::Punctuator(Punctuator::CloseBlock),
                    ],
                    next_token.clone(),
                    "object literal",
                ));
            }
        }

        Ok(Node::Object(elements))
    }
}

impl ObjectLiteral {
    /// Parses a property definition.
    ///
    /// More information:
    ///  - [ECMAScript specification][spec]
    ///
    /// [spec]: https://tc39.es/ecma262/#prod-PropertyDefinition
    fn read_property_definition(cursor: &mut Cursor<'_>) -> Result<PropertyDefinition, ParseError> {
        fn to_string(kind: &TokenKind) -> String {
            match kind {
                TokenKind::Identifier(name) => name.clone(),
                TokenKind::NumericLiteral(n) => format!("{}", n),
                TokenKind::StringLiteral(s) => s.clone(),
                _ => unimplemented!("{:?}", kind),
            }
        }

        if cursor
            .next_if_skip_lineterminator(TokenKind::Punctuator(Punctuator::Spread))
            .is_some()
        {
            let node = AssignmentExpression::parse(cursor)?;
            return Ok(PropertyDefinition::SpreadObject(node));
        }

        let prop_name = cursor
            .next_skip_lineterminator()
            .map(|tok| to_string(&tok.kind))
            .ok_or(ParseError::AbruptEnd)?;

        if cursor
            .next_if_skip_lineterminator(TokenKind::Punctuator(Punctuator::Colon))
            .is_some()
        {
            let val = AssignmentExpression::parse(cursor)?;
            return Ok(PropertyDefinition::Property(prop_name, val));
        }

        // TODO: Split into separate function: read_property_method_definition
        if cursor
            .next_if_skip_lineterminator(TokenKind::Punctuator(Punctuator::OpenParen))
            .is_some()
        {
            let params = read_formal_parameters(cursor)?;

            cursor.expect_punc(Punctuator::OpenBlock, "method definition")?;

            let body = read_statements(cursor, true).map(Node::StatementList)?;

            cursor.expect_punc(Punctuator::CloseBlock, "method definition")?;

            return Ok(PropertyDefinition::MethodDefinition(
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
        Err(ParseError::General(
            "expected property definition",
            Some(pos),
        ))
    }
}
