//! This module implements all of the [Token]s used in the JavaScript programing language.
//!
//! More information:
//!  - [ECMAScript reference][spec]
//!
//! [spec]: https://tc39.es/ecma262/#sec-tokens

use crate::syntax::ast::{keyword::Keyword, pos::Position, punc::Punctuator};
use crate::{Interner, Sym};
use std::fmt::{Debug, Display, Formatter, Result};

#[cfg(feature = "serde-ast")]
use serde::{Deserialize, Serialize};

/// This represents the smallest individual words, phrases, or characters that JavaScript can understand.
///
/// More information:
///  - [ECMAScript reference][spec]
///
/// [spec]: https://tc39.es/ecma262/#sec-tokens
#[cfg_attr(feature = "serde-ast", derive(Serialize, Deserialize))]
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token {
    /// The token kind, which contains the actual data of the token.
    pub kind: TokenKind,

    /// The token position from origina source code.
    pub pos: Position,
}

impl Token {
    /// Create a new detailed token from the token data, line number and column number
    pub fn new(kind: TokenKind, line_number: u64, column_number: u64) -> Self {
        Self {
            kind,
            pos: Position::new(line_number, column_number),
        }
    }

    /// Creates an object with the `fmt::Display` implementation for this token.
    pub fn display<'f>(self, interner: &'f Interner) -> TokenKindDisplay<'f> {
        TokenKindDisplay {
            kind: self.kind,
            interner,
        }
    }

    /// Gets the token as a symbol for an interned string.
    ///
    /// This is just an optimisation for
    /// `interner.get_or_intern(token.display(interner).to_string())`.
    pub fn to_string_sym(self, interner: &Interner) -> Sym {
        // TODO: optimise by not allocating a string for booleans, literals, nulls and so on.
        interner.get_or_intern(self.display(interner).to_string())
    }
}

/// A continuous sequence of tokens.
pub struct VecToken(Vec<Token>);
// pub struct VecToken(Vec<Token>);

// impl Debug for VecToken {
//     fn fmt(&self, f: &mut Formatter<'_>) -> Result {
//         let mut buffer = String::new();
//         for token in &self.0 {
//             buffer.push_str(&token.to_string());
//         }
//         write!(f, "{}", buffer)
//     }
// }

/// Represents the type of Token and the data it has inside.
#[cfg_attr(feature = "serde-ast", derive(Serialize, Deserialize))]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    /// A boolean literal, which is either `true` or `false`.
    BooleanLiteral(bool),

    /// The end of the file.
    EOF,

    /// An identifier.
    Identifier(Sym),

    /// A keyword.
    ///
    /// see: [`Keyword`](../keyword/enum.Keyword.html)
    Keyword(Keyword),

    /// A `null` literal.
    NullLiteral,

    /// A numeric literal.
    NumericLiteral(f64),

    /// A piece of punctuation
    ///
    /// see: [`Punctuator`](../punc/enum.Punctuator.html)
    Punctuator(Punctuator),

    /// A string literal.
    StringLiteral(Sym),

    /// A regular expression, consisting of body and flags.
    RegularExpressionLiteral(Sym, Sym),

    /// Indicates the end of a line (`\n`).
    LineTerminator,
}

impl From<bool> for TokenKind {
    fn from(oth: bool) -> Self {
        Self::BooleanLiteral(oth)
    }
}

impl From<Keyword> for TokenKind {
    fn from(kw: Keyword) -> Self {
        Self::Keyword(kw)
    }
}

impl From<Punctuator> for TokenKind {
    fn from(punc: Punctuator) -> Self {
        Self::Punctuator(punc)
    }
}

impl TokenKind {
    /// Creates a `BooleanLiteral` token kind.
    pub fn boolean_literal(lit: bool) -> Self {
        Self::BooleanLiteral(lit)
    }

    /// Creates an `EOF` token kind.
    pub fn eof() -> Self {
        Self::EOF
    }

    /// Creates an `Identifier` token type.
    pub fn identifier(ident: Sym) -> Self {
        Self::Identifier(ident)
    }

    /// Creates a `Keyword` token kind.
    pub fn keyword(keyword: Keyword) -> Self {
        Self::Keyword(keyword)
    }

    /// Creates a `NumericLiteral` token kind.
    pub fn numeric_literal(lit: f64) -> Self {
        Self::NumericLiteral(lit)
    }

    /// Creates a `Punctuator` token type.
    pub fn punctuator(punc: Punctuator) -> Self {
        Self::Punctuator(punc)
    }

    /// Creates a `StringLiteral` token type.
    pub fn string_literal(lit: Sym) -> Self {
        Self::StringLiteral(lit)
    }

    /// Creates a `RegularExpressionLiteral` token kind.
    pub fn regular_expression_literal(body: Sym, flags: Sym) -> Self {
        Self::RegularExpressionLiteral(body, flags)
    }

    /// Creates a `LineTerminator` token kind.
    pub fn line_terminator() -> Self {
        Self::LineTerminator
    }

    /// Creates an object with the `fmt::Display` implementation for this token kind.
    pub fn display<'f>(self, interner: &'f Interner) -> TokenKindDisplay<'f> {
        TokenKindDisplay {
            kind: self,
            interner,
        }
    }

    /// Gets the token as a symbol for an interned string.
    ///
    /// This is just an optimisation for
    /// `interner.get_or_intern(token_kind.display(interner).to_string())`.
    pub fn to_string_sym(self, interner: &Interner) -> Sym {
        // TODO: optimise by not allocating a string for booleans, literals, nulls and so on.
        interner.get_or_intern(self.display(interner).to_string())
    }
}

/// Structure implementing the `fmt::Display` trait for a `TokenKind`.
#[derive(Debug)]
pub struct TokenKindDisplay<'i> {
    kind: TokenKind,
    interner: &'i Interner,
}

impl Display for TokenKindDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self.kind {
            TokenKind::BooleanLiteral(ref val) => write!(f, "{}", val),
            TokenKind::EOF => write!(f, "end of file"),
            TokenKind::Identifier(ident) => write!(
                f,
                "{}",
                self.interner
                    .resolve(ident)
                    .expect("identifier string not found")
            ),
            TokenKind::Keyword(ref word) => write!(f, "{}", word),
            TokenKind::NullLiteral => write!(f, "null"),
            TokenKind::NumericLiteral(ref num) => write!(f, "{}", num),
            TokenKind::Punctuator(ref punc) => write!(f, "{}", punc),
            TokenKind::StringLiteral(lit) => write!(
                f,
                "{}",
                self.interner
                    .resolve(lit)
                    .expect("could not find string literal string")
            ),
            TokenKind::RegularExpressionLiteral(body, flags) => write!(
                f,
                "/{}/{}",
                self.interner
                    .resolve(body)
                    .expect("could not find regular expression body string"),
                self.interner
                    .resolve(flags)
                    .expect("could not find regular expression flags string")
            ),
            TokenKind::LineTerminator => write!(f, "line terminator"),
        }
    }
}
