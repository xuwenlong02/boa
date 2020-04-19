//! <https://tc39.es/ecma262/#sec-declarations-and-the-variable-statement>

#[cfg(test)]
mod tests;
mod variable_statement;

use crate::syntax::{
    ast::{keyword::Keyword, node::Node, punc::Punctuator, token::TokenKind},
    parser::{
        AllowAwait, AllowIn, AllowYield, Cursor, FunctionDeclaration, Initializer, ParseError,
        ParseResult, TokenParser,
    },
};
pub(super) use variable_statement::{VariableDeclarationList, VariableStatement};

/// Parses a declaration.
///
/// More information:
///  - [ECMAScript specification][spec]
///
/// [spec]: https://tc39.es/ecma262/#prod-Declaration
#[derive(Debug, Clone, Copy)]
pub(super) struct Declaration {
    allow_yield: AllowYield,
    allow_await: AllowAwait,
}

impl Declaration {
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

impl TokenParser for Declaration {
    type Output = Node;

    fn parse(self, cursor: &mut Cursor<'_>) -> ParseResult {
        let tok = cursor
            .peek_skip_lineterminator()
            .ok_or(ParseError::AbruptEnd)?;

        match tok.kind {
            TokenKind::Keyword(Keyword::Function) => {
                // HoistableDeclaration
                FunctionDeclaration::new(self.allow_yield, self.allow_await, false).parse(cursor)
            }
            TokenKind::Keyword(Keyword::Const) | TokenKind::Keyword(Keyword::Let) => {
                LexicalDeclaration::new(true, self.allow_yield, self.allow_await).parse(cursor)
            }
            _ => unreachable!("unknown token found"),
        }
    }
}

/// <https://tc39.es/ecma262/#prod-LexicalDeclaration>
#[derive(Debug, Clone, Copy)]
struct LexicalDeclaration {
    allow_in: AllowIn,
    allow_yield: AllowYield,
    allow_await: AllowAwait,
}

impl LexicalDeclaration {
    /// Creates a new `LexicalDeclaration` parser.
    fn new<I, Y, A>(allow_in: I, allow_yield: Y, allow_await: A) -> Self
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

impl TokenParser for LexicalDeclaration {
    type Output = Node;

    fn parse(self, cursor: &mut Cursor<'_>) -> ParseResult {
        let tok = cursor
            .next_skip_lineterminator()
            .ok_or(ParseError::AbruptEnd)?;

        match tok.kind {
            TokenKind::Keyword(Keyword::Const) => {
                BindingList::new(self.allow_in, self.allow_yield, self.allow_await, true)
                    .parse(cursor)
            }
            TokenKind::Keyword(Keyword::Let) => {
                BindingList::new(self.allow_in, self.allow_yield, self.allow_await, false)
                    .parse(cursor)
            }
            _ => unreachable!("unknown token found"),
        }
    }
}

/// Parses a binding list.
///
/// It will return an error if a `const` declaration is being parsed and there is no
/// initializer.
///
/// More information: <https://tc39.es/ecma262/#prod-BindingList>.
#[derive(Debug, Clone, Copy)]
struct BindingList {
    allow_in: AllowIn,
    allow_yield: AllowYield,
    allow_await: AllowAwait,
    is_const: bool,
}

impl BindingList {
    /// Creates a new `BindingList` parser.
    fn new<I, Y, A>(allow_in: I, allow_yield: Y, allow_await: A, is_const: bool) -> Self
    where
        I: Into<AllowIn>,
        Y: Into<AllowYield>,
        A: Into<AllowAwait>,
    {
        Self {
            allow_in: allow_in.into(),
            allow_yield: allow_yield.into(),
            allow_await: allow_await.into(),
            is_const,
        }
    }
}

impl TokenParser for BindingList {
    type Output = Node;

    fn parse(self, cursor: &mut Cursor<'_>) -> ParseResult {
        // Create vectors to store the variable declarations
        // Const and Let signatures are slightly different, Const needs definitions, Lets don't
        let mut let_decls = Vec::new();
        let mut const_decls = Vec::new();

        loop {
            let token = cursor
                .next_skip_lineterminator()
                .ok_or(ParseError::AbruptEnd)?;
            let name = if let TokenKind::Identifier(ref name) = token.kind {
                name.clone()
            } else {
                return Err(ParseError::Expected(
                    vec![TokenKind::identifier("identifier")],
                    token.clone(),
                    if self.is_const {
                        "const declaration"
                    } else {
                        "let declaration"
                    },
                ));
            };

            cursor.skip(|tk| tk.kind == TokenKind::LineTerminator);
            match cursor.peek(0) {
                Some(token) if token.kind == TokenKind::Punctuator(Punctuator::Assign) => {
                    let init = Some(
                        Initializer::new(self.allow_in, self.allow_yield, self.allow_await)
                            .parse(cursor)?,
                    );
                    if self.is_const {
                        const_decls.push((name, init.unwrap()));
                    } else {
                        let_decls.push((name, init));
                    };
                }
                _ => {
                    if self.is_const {
                        return Err(ParseError::Expected(
                            vec![TokenKind::Punctuator(Punctuator::Assign)],
                            cursor
                                .next_skip_lineterminator()
                                .ok_or(ParseError::AbruptEnd)?
                                .clone(),
                            "const declaration",
                        ));
                    } else {
                        let_decls.push((name, None));
                    }
                }
            }

            if !lexical_declaration_continuation(cursor)? {
                break;
            }
        }

        if self.is_const {
            Ok(Node::ConstDecl(const_decls))
        } else {
            Ok(Node::LetDecl(let_decls))
        }
    }
}

/// Checks if the lexical declaration continues with more bindings.
///
/// If it does, it will advance the internal cursor to the next identifier token.
/// A Lexical Declaration continues its binding list if we find a `,` character. A New line
/// indicates the same as a `;`.
///
/// More information: <https://tc39.es/ecma262/#prod-LexicalDeclaration>.
fn lexical_declaration_continuation(cursor: &mut Cursor<'_>) -> Result<bool, ParseError> {
    if let Some(tok) = cursor.peek(0) {
        match tok.kind {
            TokenKind::LineTerminator | TokenKind::Punctuator(Punctuator::Semicolon) => Ok(false),
            TokenKind::Punctuator(Punctuator::Comma) => {
                let _ = cursor
                    .next_skip_lineterminator()
                    .ok_or(ParseError::AbruptEnd)?;
                Ok(true)
            }
            _ => Err(ParseError::Expected(
                vec![
                    TokenKind::Punctuator(Punctuator::Semicolon),
                    TokenKind::LineTerminator,
                ],
                cursor.next().ok_or(ParseError::AbruptEnd)?.clone(),
                "lexical declaration",
            )),
        }
    } else {
        Ok(false)
    }
}
