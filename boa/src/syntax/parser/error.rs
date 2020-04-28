//! Error and result implementation for the parser.
use crate::{
    syntax::ast::{keyword::Keyword, node::Node, pos::Position},
    Interner,
};
use std::fmt;

/// Result of a parsing operation.
pub type ParseResult = Result<Node, ParseError>;

/// `ParseError` is an enum which represents errors encounted during parsing an expression
#[derive(Debug, Clone)]
pub enum ParseError {
    /// When it expected a certain kind of token, but got another as part of something.
    Expected {
        expected: Box<[String]>,
        actual: String,
        pos: Position,
        routine: &'static str,
    },
    /// When it expected a certain expression, but got another.
    ExpectedExpr {
        expected: &'static str,
        found: String,
        pos: Position,
    },
    /// When it didn't expect this keyword.
    UnexpectedKeyword { keyword: Keyword, pos: Position },
    /// When a token is unexpected.
    Unexpected {
        tok: String,
        pos: Position,
        msg: Option<&'static str>,
    },
    /// When there is an abrupt end to the parsing
    AbruptEnd,
    /// Out of range error, attempting to set a position where there is no token
    RangeError,
    /// Catch all General Error
    General { msg: &'static str, pos: Position },
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Expected {
                expected,
                actual,
                pos,
                routine,
            } => write!(
                f,
                "Expected {}, got '{}' in {} at line {}, col {}",
                if let Some(expected) = expected.first() {
                    format!("token '{}'", expected)
                } else {
                    format!(
                        "one of {}",
                        expected
                            .iter()
                            .enumerate()
                            .map(|(i, t)| {
                                format!(
                                    "{}'{}'",
                                    if i == 0 {
                                        ""
                                    } else if i == expected.len() - 1 {
                                        " or "
                                    } else {
                                        ", "
                                    },
                                    t
                                )
                            })
                            .collect::<String>()
                    )
                },
                actual,
                routine,
                pos.line_number,
                pos.column_number
            ),
            Self::ExpectedExpr {
                expected,
                found,
                pos,
            } => write!(
                f,
                "Expected expression '{}', got '{}' at line {}, col {}",
                expected, found, pos.line_number, pos.column_number
            ),
            Self::UnexpectedKeyword { keyword, pos } => write!(
                f,
                "Unexpected keyword: '{}' at line {}, col {}",
                keyword, pos.line_number, pos.column_number
            ),
            Self::Unexpected { tok, pos, msg } => write!(
                f,
                "Unexpected Token '{}'{} at line {}, col {}",
                tok,
                if let Some(m) = msg {
                    format!(", {}", m)
                } else {
                    String::new()
                },
                pos.line_number,
                pos.column_number
            ),
            Self::AbruptEnd => write!(f, "Abrupt End"),
            Self::General { msg, pos } => write!(
                f,
                "{} at line {}, col {}",
                msg, pos.line_number, pos.column_number
            ),
            Self::RangeError => write!(f, "RangeError!"),
        }
    }
}

impl ParseError {
    /// Creates a `ParseError::Expected` error.
    pub(super) fn expected<E, A>(
        expected: E,
        actual: A,
        pos: Position,
        routine: &'static str,
    ) -> Self
    where
        E: Into<Box<[String]>>,
        A: Into<String>,
    {
        Self::Expected {
            expected: expected.into(),
            actual: actual.into(),
            pos,
            routine,
        }
    }

    /// Creates a `ParseError::UnexpectedKeyword` error.
    pub(super) fn unexpected_keyword(keyword: Keyword, pos: Position) -> Self {
        Self::UnexpectedKeyword { keyword, pos }
    }

    /// Creates `ParseError::Unexpected` error.
    pub(super) fn unexpected<T, M>(tok: T, pos: Position, msg: M) -> Self
    where
        T: Into<String>,
        M: Into<Option<&'static str>>,
    {
        Self::Unexpected {
            tok: tok.into(),
            pos,
            msg: msg.into(),
        }
    }

    /// Creates a `ParseError::AbruptEnd` error.
    pub(super) fn abrupt_end() -> Self {
        Self::AbruptEnd
    }

    /// Creates a `ParseError::RangeError` error.
    pub(super) fn range_error() -> Self {
        Self::RangeError
    }

    /// Creates a `ParseError::General` error.
    pub fn general(msg: &'static str, pos: Position) -> Self {
        Self::General { msg, pos }
    }
}
