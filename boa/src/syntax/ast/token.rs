use crate::{
    syntax::ast::{keyword::Keyword, pos::Position, punc::Punctuator},
    Interner, InternerSym,
};
use std::fmt::{Debug, Display, Formatter, Result};

#[cfg(feature = "serde-ast")]
use serde::{Deserialize, Serialize};

/// Represents a token.
#[cfg_attr(feature = "serde-ast", derive(Serialize, Deserialize))]
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token {
    /// The token Data
    pub kind: TokenKind,
    /// Token position from original source code
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
    pub fn display<'f>(&'f self, interner: &'f Interner) -> TokenDisplay<'f, 'f> {
        TokenDisplay {
            token: &self,
            interner,
        }
    }
}

/// Structure implementing the `fmt::Display` trait for a `Token`.
#[derive(Debug)]
struct TokenDisplay<'t, 'i> {
    token: &'t Token,
    interner: &'i Interner,
}

impl Display for TokenDisplay<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.token.kind.display(self.interner))
    }
}

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
    /// A boolean literal, which is either `true` or `false`
    BooleanLiteral(bool),
    /// The end of the file
    EOF,
    /// An identifier
    Identifier(InternerSym),
    /// A keyword
    Keyword(Keyword),
    /// A `null` literal
    NullLiteral,
    /// A numeric literal
    NumericLiteral(f64),
    /// A piece of punctuation
    Punctuator(Punctuator),
    /// A string literal
    StringLiteral(InternerSym),
    /// A regular expression, consisting of body and flags
    RegularExpressionLiteral(InternerSym, InternerSym),
    /// Indicates the end of a line \n
    LineTerminator,
}

impl TokenKind {
    /// Creates an object with the `fmt::Display` implementation for this token kind.
    pub fn display<'f>(self, interner: &'f Interner) -> TokenKindDisplay<'f> {
        TokenKindDisplay {
            kind: self,
            interner,
        }
    }
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
