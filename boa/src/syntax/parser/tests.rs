//! Tests for the parser.

use super::Parser;
use crate::{
    syntax::{ast::node::Node, ast::op::NumOp, lexer::Lexer},
    Interner,
};

/// Checks that the given string parses to the given list of nodes.
#[allow(clippy::result_unwrap_used)]
pub(super) fn check_parser(js: &str, expr: &[Node], interner: Interner) {
    let mut lexer = Lexer::new_with_interner(js, interner);
    lexer.lex().expect("failed to lex");

    assert_eq!(
        Parser::new(&lexer.tokens, lexer.interner)
            .parse_all()
            .expect("failed to parse"),
        Node::statement_list(expr)
    );
}

/// Checks that the given string failes to parse.
pub(super) fn check_invalid(js: &str) {
    let mut lexer = Lexer::new(js);
    lexer.lex().expect("failed to lex");

    assert!(Parser::new(&lexer.tokens, lexer.interner)
        .parse_all()
        .is_err());
}

/// Should be parsed as `new Class().method()` instead of `new (Class().method())`
#[test]
fn check_construct_call_precedence() {
    check_parser(
        "new Date().getTime()",
        &[Node::call(
            Node::get_const_field(
                Node::new(Node::call(Node::local("Date"), Vec::new())),
                "getTime",
            ),
            Vec::new(),
        )],
    );
}

#[test]
fn assing_operator_precedence() {
    check_parser(
        "a = a + 1",
        &[Node::assign(
            Node::local("a"),
            Node::bin_op(NumOp::Add, Node::local("a"), Node::const_node(1.0)),
        )],
    );
}
