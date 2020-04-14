//! Expression parsing.
//!
//! More information:
//!  - [MDN documentation][mdn]
//!  - [ECMAScript specification][spec]
//!
//! [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators
//! [spec]: https://tc39.es/ecma262/#sec-ecmascript-language-expressions

mod array_initializer;
mod assignment_operator;
mod conditional_operator;
mod exponentiation_operator;
mod lhs_expression;
mod object_initializer;
mod primary_expression;
mod unary_operator;
mod update_expression;

pub(super) use self::assignment_operator::AssignmentExpression;
use self::exponentiation_operator::ExponentiationExpression;
use super::{Cursor, ParseResult, TokenParser};
use crate::syntax::ast::{node::Node, punc::Punctuator, token::TokenKind};

/// Generates an expression parser.
///
/// This macro has 2 mandatory identifiers:
///  - The `$name` identifier will contain the name of the parsing structure.
///  - The `$lower` identifier will contain the parser for lower level expressions.
///
/// Those exressions are divided by the punctuators passed as the third parameter.
macro_rules! expression { ($name:ident, $lower:ident, [$( $op:path ),*] ) => {
    impl TokenParser for $name {
        fn parse(cursor: &mut Cursor<'_>) -> ParseResult {
            let mut lhs = $lower::parse(cursor)?;
            while let Some(tok) = cursor.peek_skip_lineterminator() {
                match tok.kind {
                    TokenKind::Punctuator(op) if $( op == $op )||* => {
                        let _ = cursor.next_skip_lineterminator().expect("token disappeared");
                        lhs = Node::bin_op(
                            op.as_binop().expect("could not get binary operation"),
                            lhs,
                            $lower::parse(cursor)?
                        )
                    }
                    _ => break
                }
            }
            Ok(lhs)
        }
    }
} }

/// Expression parsing.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators
/// [spec]: https://tc39.es/ecma262/#prod-Expression
#[derive(Debug, Clone, Copy)]
pub(super) struct Expression;

expression!(Expression, AssignmentExpression, [Punctuator::Comma]);

/// Parses a logical `OR` expression.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Logical_Operators#Logical_OR_2
/// [spec]: https://tc39.es/ecma262/#prod-LogicalORExpression
#[derive(Debug, Clone, Copy)]
struct LogicalORExpression;

expression!(
    LogicalORExpression,
    LogicalANDExpression,
    [Punctuator::BoolOr]
);

/// Parses a logical `AND` expression.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Logical_Operators#Logical_AND_2
/// [spec]: https://tc39.es/ecma262/#prod-LogicalANDExpression
#[derive(Debug, Clone, Copy)]
struct LogicalANDExpression;

expression!(
    LogicalANDExpression,
    BitwiseORExpression,
    [Punctuator::BoolAnd]
);

/// Parses a bitwise `OR` expression.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Bitwise_Operators#Bitwise_OR
/// [spec]: https://tc39.es/ecma262/#prod-BitwiseORExpression
#[derive(Debug, Clone, Copy)]
struct BitwiseORExpression;

expression!(BitwiseORExpression, BitwiseXORExpression, [Punctuator::Or]);

/// Parses a bitwise `XOR` expression.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Bitwise_Operators#Bitwise_XOR
/// [spec]: https://tc39.es/ecma262/#prod-BitwiseXORExpression
#[derive(Debug, Clone, Copy)]
struct BitwiseXORExpression;

expression!(
    BitwiseXORExpression,
    BitwiseANDExpression,
    [Punctuator::Xor]
);

/// Parses a bitwise `AND` expression.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Bitwise_Operators#Bitwise_AND
/// [spec]: https://tc39.es/ecma262/#prod-BitwiseANDExpression
#[derive(Debug, Clone, Copy)]
struct BitwiseANDExpression;

expression!(BitwiseANDExpression, EqualityExpression, [Punctuator::And]);

/// Parses an equality expression.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Comparison_Operators#Equality_operators
/// [spec]: https://tc39.es/ecma262/#sec-equality-operators
#[derive(Debug, Clone, Copy)]
struct EqualityExpression;

expression!(
    EqualityExpression,
    RelationalExpression,
    [
        Punctuator::Eq,
        Punctuator::NotEq,
        Punctuator::StrictEq,
        Punctuator::StrictNotEq
    ]
);

/// Parses a relational expression.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Comparison_Operators#Relational_operators
/// [spec]: https://tc39.es/ecma262/#sec-relational-operators
#[derive(Debug, Clone, Copy)]
struct RelationalExpression;

expression!(
    RelationalExpression,
    ShiftExpression,
    [
        Punctuator::LessThan,
        Punctuator::GreaterThan,
        Punctuator::LessThanOrEq,
        Punctuator::GreaterThanOrEq
    ]
);

/// Parses a bitwise shift expression.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Bitwise_Operators#Bitwise_shift_operators
/// [spec]: https://tc39.es/ecma262/#sec-bitwise-shift-operators
#[derive(Debug, Clone, Copy)]
struct ShiftExpression;

expression!(
    ShiftExpression,
    AdditiveExpression,
    [
        Punctuator::LeftSh,
        Punctuator::RightSh,
        Punctuator::URightSh
    ]
);

/// Parses an additive expression.
///
/// This can be either an addition or a subtraction.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Arithmetic_Operators
/// [spec]: https://tc39.es/ecma262/#sec-additive-operators
#[derive(Debug, Clone, Copy)]
struct AdditiveExpression;

expression!(
    AdditiveExpression,
    MultiplicativeExpression,
    [Punctuator::Add, Punctuator::Sub]
);

/// Parses a multiplicative expression.
///
/// This can be either a multiplication, division or a modulo (remainder) expression.
///
/// More information:
///  - [MDN documentation][mdn]
///  - [ECMAScript specification][spec]
///
/// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Arithmetic_Operators#Division
/// [spec]: https://tc39.es/ecma262/#sec-multiplicative-operators
#[derive(Debug, Clone, Copy)]
struct MultiplicativeExpression;

expression!(
    MultiplicativeExpression,
    ExponentiationExpression,
    [Punctuator::Mul, Punctuator::Div, Punctuator::Mod]
);
