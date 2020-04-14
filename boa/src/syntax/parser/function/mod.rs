//! Functions and classes parsing.
//!
//! More information:
//!  - [MDN documentation][mdn]
//!  - [ECMAScript specification][spec]
//!
//! [mdn]: https://developer.mozilla.org/en-US/docs/Glossary/Function
//! [spec]: https://tc39.es/ecma262/#sec-ecmascript-language-functions-and-classes

mod arrow_function;
mod function_definition;

use super::{Cursor, ParseError};
use crate::syntax::ast::{
    node::{FormalParameter, FormalParameters},
    punc::Punctuator,
    token::TokenKind,
};
pub(super) use {arrow_function::ArrowFunction, function_definition::FunctionExpression};

/// Formal parameters parsing.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Glossary/Parameter
/// [spec]: https://tc39.es/ecma262/#prod-FormalParameters
pub(super) fn read_formal_parameters(
    cursor: &mut Cursor<'_>,
) -> Result<FormalParameters, ParseError> {
    let mut params = Vec::new();

    if cursor
        .next_if_skip_lineterminator(TokenKind::Punctuator(Punctuator::CloseParen))
        .is_some()
    {
        return Ok(params);
    }

    loop {
        let mut rest_param = false;

        params.push(
            if cursor
                .next_if_skip_lineterminator(TokenKind::Punctuator(Punctuator::Spread))
                .is_some()
            {
                rest_param = true;
                read_function_rest_parameter(cursor)?
            } else {
                read_formal_parameter(cursor)?
            },
        );

        if cursor
            .next_if(TokenKind::Punctuator(Punctuator::CloseParen))
            .is_some()
        {
            break;
        }

        if rest_param {
            return Err(ParseError::Unexpected(
                cursor
                    .peek_prev()
                    .expect("current token disappeared")
                    .clone(),
                Some("rest parameter must be the last formal parameter"),
            ));
        }

        cursor.expect_punc(Punctuator::Comma, "parameter list")?;
    }

    Ok(params)
}

/// Rest parameter parsing.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/rest_parameters
/// [spec]: https://tc39.es/ecma262/#prod-FunctionRestParameter
fn read_function_rest_parameter(cursor: &mut Cursor<'_>) -> Result<FormalParameter, ParseError> {
    let token = cursor.next().ok_or(ParseError::AbruptEnd)?;
    Ok(FormalParameter::new(
        if let TokenKind::Identifier(name) = &token.kind {
            name
        } else {
            return Err(ParseError::Expected(
                vec![TokenKind::identifier("identifier")],
                token.clone(),
                "rest parameter",
            ));
        },
        None,
        true,
    ))
}

/// Formal parameter parsing.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Glossary/Parameter
/// [spec]: https://tc39.es/ecma262/#prod-FormalParameter
fn read_formal_parameter(cursor: &mut Cursor<'_>) -> Result<FormalParameter, ParseError> {
    let token = cursor
        .next_skip_lineterminator()
        .ok_or(ParseError::AbruptEnd)?;
    let name = if let TokenKind::Identifier(name) = &token.kind {
        name
    } else {
        return Err(ParseError::Expected(
            vec![TokenKind::identifier("identifier")],
            token.clone(),
            "formal parameter",
        ));
    };

    // TODO: Implement initializer.
    Ok(FormalParameter::new(name, None, false))
}
