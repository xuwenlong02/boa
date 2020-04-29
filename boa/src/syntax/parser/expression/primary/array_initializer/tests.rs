// ! Tests for array initializer parsing.

use crate::{
    syntax::{
        ast::{constant::Const, node::Node},
        parser::tests::check_parser,
    },
    Interner,
};

/// Checks an empty array.
#[test]
fn check_empty() {
    let mut int = Interner::new();
    check_parser("[]", &[Node::ArrayDecl(Vec::new())], int);
}

/// Checks an array with empty slot.
#[test]
fn check_empty_slot() {
    let mut int = Interner::new();
    check_parser(
        "[,]",
        &[Node::ArrayDecl(vec![Node::Const(Const::Undefined)])],
        int,
    );
}

/// Checks a numeric array.
#[test]
fn check_numeric_array() {
    let mut int = Interner::new();
    check_parser(
        "[1, 2, 3]",
        &[Node::ArrayDecl(vec![
            Node::const_node(1.0),
            Node::const_node(2.0),
            Node::const_node(3.0),
        ])],
        int,
    );
}

// Checks a numeric array with trailing comma
#[test]
fn check_numeric_array_trailing() {
    let mut int = Interner::new();
    check_parser(
        "[1, 2, 3,]",
        &[Node::ArrayDecl(vec![
            Node::const_node(1.0),
            Node::const_node(2.0),
            Node::const_node(3.0),
        ])],
        int,
    );
}

/// Checks a numeric array with an elision.
#[test]
fn check_numeric_array_elision() {
    let mut int = Interner::new();
    check_parser(
        "[1, 2, , 3]",
        &[Node::ArrayDecl(vec![
            Node::const_node(1.0),
            Node::const_node(2.0),
            Node::Const(Const::Undefined),
            Node::const_node(3.0),
        ])],
        int,
    );
}

/// Checks a numeric array with repeated elisions.
#[test]
fn check_numeric_array_repeated_elision() {
    let mut int = Interner::new();
    check_parser(
        "[1, 2, ,, 3]",
        &[Node::ArrayDecl(vec![
            Node::const_node(1.0),
            Node::const_node(2.0),
            Node::Const(Const::Undefined),
            Node::Const(Const::Undefined),
            Node::const_node(3.0),
        ])],
        int,
    );
}

/// Checks a combined array.
#[test]
fn check_combined() {
    let mut int = Interner::new();
    check_parser(
        "[1, \"a\", 2]",
        &[Node::ArrayDecl(vec![
            Node::const_node(1.0),
            Node::const_node(int.get_or_intern("a")),
            Node::const_node(2.0),
        ])],
        int,
    );
}

/// Checks a combined array with an empty string
#[test]
fn check_combined_empty_str() {
    let mut int = Interner::new();
    check_parser(
        "[1, \"\", 2]",
        &[Node::ArrayDecl(vec![
            Node::const_node(1.0),
            Node::const_node(int.get_or_intern("")),
            Node::const_node(2.0),
        ])],
        int,
    );
}
