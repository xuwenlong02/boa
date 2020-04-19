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

pub(in crate::syntax::parser) use self::{
    arrow_function::ArrowFunction,
    function_definition::{
        FormalParameters, FunctionBody, FunctionDeclaration, FunctionExpression,
    },
};
