use crate::{
    syntax::{
        ast::node::Node,
        parser::tests::{check_invalid, check_parser},
    },
    Interner,
};

/// Checks `var` declaration parsing.
#[test]
fn check_var_declaration() {
    let mut int = Interner::new();

    check_parser(
        "var a = 5;",
        &[Node::VarDecl(vec![(
            int.get_or_intern("a"),
            Some(Node::const_node(5.0)),
        )])],
        int,
    );
}

/// Checks `var` declaration parsing with no spaces.
#[test]
fn check_var_declaration_no_spaces() {
    let mut int = Interner::new();

    check_parser(
        "var a=5;",
        &[Node::VarDecl(vec![(
            int.get_or_intern("a"),
            Some(Node::const_node(5.0)),
        )])],
        int,
    );
}

/// Checks empty `var` declaration parsing.
#[test]
fn check_empty_var_declaration() {
    let mut int = Interner::new();

    check_parser(
        "var a;",
        &[Node::VarDecl(vec![(int.get_or_intern("a"), None)])],
        int,
    );
}

/// Checks multiple `var` declarations.
#[test]
fn check_multiple_var_declaration() {
    let mut int = Interner::new();

    check_parser(
        "var a = 5, b, c = 6;",
        &[Node::VarDecl(vec![
            (int.get_or_intern("a"), Some(Node::const_node(5.0))),
            (int.get_or_intern("b"), None),
            (int.get_or_intern("c"), Some(Node::const_node(6.0))),
        ])],
        int,
    );
}

/// Checks `let` declaration parsing.
#[test]
fn check_let_declaration() {
    let mut int = Interner::new();

    check_parser(
        "let a = 5;",
        &[Node::LetDecl(vec![(
            int.get_or_intern("a"),
            Some(Node::const_node(5.0)),
        )])],
        int,
    );
}

/// Checks `let` declaration parsing with no spaces.
#[test]
fn check_let_declaration_no_spaces() {
    let mut int = Interner::new();

    check_parser(
        "let a=5;",
        &[Node::LetDecl(vec![(
            int.get_or_intern("a"),
            Some(Node::const_node(5.0)),
        )])],
        int,
    );
}

/// Checks empty `let` declaration parsing.
#[test]
fn check_empty_let_declaration() {
    let mut int = Interner::new();

    check_parser(
        "let a;",
        &[Node::LetDecl(vec![(int.get_or_intern("a"), None)])],
        int,
    );
}

/// Checks multiple `let` declarations.
#[test]
fn check_multiple_let_declaration() {
    let mut int = Interner::new();

    check_parser(
        "let a = 5, b, c = 6;",
        &[Node::LetDecl(vec![
            (int.get_or_intern("a"), Some(Node::const_node(5.0))),
            (int.get_or_intern("b"), None),
            (int.get_or_intern("c"), Some(Node::const_node(6.0))),
        ])],
        int,
    );
}

/// Checks `const` declaration parsing.
#[test]
fn check_const_declaration() {
    let mut int = Interner::new();

    check_parser(
        "const a = 5;",
        &[Node::ConstDecl(vec![(
            int.get_or_intern("a"),
            Node::const_node(5.0),
        )])],
        int,
    );
}

/// Checks `const` declaration parsing with no spaces.
#[test]
fn check_const_declaration_no_spaces() {
    let mut int = Interner::new();

    check_parser(
        "const a=5;",
        &[Node::ConstDecl(vec![(
            int.get_or_intern("a"),
            Node::const_node(5.0),
        )])],
        int,
    );
}

/// Checks empty `const` declaration parsing.
#[test]
fn check_empty_const_declaration() {
    check_invalid("const a;");
}

/// Checks multiple `const` declarations.
#[test]
fn check_multiple_const_declaration() {
    let mut int = Interner::new();

    check_parser(
        "const a = 5, c = 6;",
        &[Node::ConstDecl(vec![
            (int.get_or_intern("a"), Node::const_node(5.0)),
            (int.get_or_intern("c"), Node::const_node(6.0)),
        ])],
        int,
    );
}
