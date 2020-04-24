use crate::{Interner, InternerSym};
use gc::{Finalize, Trace};
use std::fmt::{Display, Formatter, Result};

#[cfg(feature = "serde-ast")]
use serde::{Deserialize, Serialize};

/// A Javascript Constant.
#[cfg_attr(feature = "serde-ast", derive(Serialize, Deserialize))]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Const {
    /// A UTF-8 string, such as `"Hello, world"`
    String(InternerSym),
    // A 64-bit floating-point number, such as `3.1415`
    Num(f64),
    // A 32-bit integer, such as `42`
    Int(i32),
    // A boolean, which is either `true` or `false` and is used to check if criteria are met
    Bool(bool),
    // The `null` value, which represents a non-existant value
    Null,
    // The `undefined` value, which represents a field or index that doesn't exist
    Undefined,
}

impl Finalize for Const {}
unsafe impl Trace for Const {
    #[inline]
    unsafe fn trace(&self) {}
    #[inline]
    unsafe fn root(&self) {}
    #[inline]
    unsafe fn unroot(&self) {}
    #[inline]
    fn finalize_glue(&self) {
        Finalize::finalize(self)
    }
}

impl From<InternerSym> for Const {
    fn from(s: InternerSym) -> Self {
        Const::String(s)
    }
}

impl From<f64> for Const {
    fn from(num: f64) -> Self {
        Self::Num(num)
    }
}

impl From<i32> for Const {
    fn from(i: i32) -> Self {
        Self::Int(i)
    }
}

impl From<bool> for Const {
    fn from(b: bool) -> Self {
        Self::Bool(b)
    }
}

impl Const {
    /// Generates a structure that implements `fmt::Display` for this structure.
    pub fn display<'f>(self, interner: &'f Interner) -> ConstDisplay<'f> {
        ConstDisplay {
            constant: self,
            interner,
        }
    }
}

/// structure implementing `fmt::Display` for `Const`.
struct ConstDisplay<'d> {
    constant: Const,
    interner: &'d Interner,
}

impl Display for ConstDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self.constant {
            Const::String(st) => write!(
                f,
                "\"{}\"",
                self.interner.resolve(st).expect("string disappeared")
            ),
            Const::Num(num) => write!(f, "{}", num),
            Const::Int(num) => write!(f, "{}", num),
            Const::Bool(v) => write!(f, "{}", v),
            Const::Null => write!(f, "null"),
            Const::Undefined => write!(f, "undefined"),
        }
    }
}
