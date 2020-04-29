use crate::{
    syntax::{
        ast::node::{FormalParameter, Node},
        ast::op::NumOp,
        parser::tests::check_parser,
    },
    Interner,
};

/// Checks basic function declaration parsing.
#[test]
fn check_basic() {
    let mut int = Interner::new();
    let a_sym = int.get_or_intern("a");
    check_parser(
        "function foo(a) { return a; }",
        &[Node::function_decl(
            int.get_or_intern("foo"),
            vec![FormalParameter::new(a_sym, None, false)],
            Node::StatementList(vec![Node::return_node(Node::local(a_sym))]),
        )],
        int,
    );
}

/// Checks basic function declaration parsing with automatic semicolon insertion.
#[test]
fn check_basic_semicolon_insertion() {
    let mut int = Interner::new();
    let a_sym = int.get_or_intern("a");
    check_parser(
        "function foo(a) { return a }",
        &[Node::function_decl(
            int.get_or_intern("foo"),
            vec![FormalParameter::new(a_sym, None, false)],
            Node::StatementList(vec![Node::return_node(Node::local(a_sym))]),
        )],
        int,
    );
}

/// Checks functions with empty returns.
#[test]
fn check_empty_return() {
    let mut int = Interner::new();
    check_parser(
        "function foo(a) { return; }",
        &[Node::function_decl(
            int.get_or_intern("foo"),
            vec![FormalParameter::new(int.get_or_intern("a"), None, false)],
            Node::StatementList(vec![Node::Return(None)]),
        )],
        int,
    );
}

/// Checks functions with empty returns without semicolon
#[test]
fn check_empty_return_semicolon_insertion() {
    let mut int = Interner::new();
    check_parser(
        "function foo(a) { return }",
        &[Node::function_decl(
            int.get_or_intern("foo"),
            vec![FormalParameter::new(int.get_or_intern("a"), None, false)],
            Node::StatementList(vec![Node::Return(None)]),
        )],
        int,
    );
}

/// Checks rest operator parsing.
#[test]
fn check_rest_operator() {
    let mut int = Interner::new();
    check_parser(
        "function foo(a, ...b) {}",
        &[Node::function_decl(
            int.get_or_intern("foo"),
            vec![
                FormalParameter::new(int.get_or_intern("a"), None, false),
                FormalParameter::new(int.get_or_intern("b"), None, true),
            ],
            Node::StatementList(Vec::new()),
        )],
        int,
    );
}

/// Checks an arrow function with only a rest parameter.
#[test]
fn check_arrow_only_rest() {
    let mut int = Interner::new();
    check_parser(
        "(...a) => {}",
        &[Node::arrow_function_decl(
            vec![FormalParameter::new(int.get_or_intern("a"), None, true)],
            Node::StatementList(Vec::new()),
        )],
        int,
    );
}

/// Checks an arrow function with a rest parameter.
#[test]
fn check_arrow_rest() {
    let mut int = Interner::new();
    check_parser(
        "(a, b, ...c) => {}",
        &[Node::arrow_function_decl(
            vec![
                FormalParameter::new(int.get_or_intern("a"), None, false),
                FormalParameter::new(int.get_or_intern("b"), None, false),
                FormalParameter::new(int.get_or_intern("c"), None, true),
            ],
            Node::StatementList(Vec::new()),
        )],
        int,
    );
}

/// Checks an arrow function with expression return.
#[test]
fn check_arrow() {
    let mut int = Interner::new();
    let a_sym = int.get_or_intern("a");
    let b_sym = int.get_or_intern("b");
    check_parser(
        "(a, b) => { return a + b; }",
        &[Node::arrow_function_decl(
            vec![
                FormalParameter::new(a_sym, None, false),
                FormalParameter::new(b_sym, None, false),
            ],
            Node::StatementList(vec![Node::return_node(Node::bin_op(
                NumOp::Add,
                Node::local(a_sym),
                Node::local(b_sym),
            ))]),
        )],
        int,
    );
}

/// Checks an arrow function with expression return and automatic semicolon insertion
#[test]
fn check_arrow_semicolon_insertion() {
    let mut int = Interner::new();
    let a_sym = int.get_or_intern("a");
    let b_sym = int.get_or_intern("b");
    check_parser(
        "(a, b) => { return a + b }",
        &[Node::arrow_function_decl(
            vec![
                FormalParameter::new(a_sym, None, false),
                FormalParameter::new(b_sym, None, false),
            ],
            Node::StatementList(vec![Node::return_node(Node::bin_op(
                NumOp::Add,
                Node::local(a_sym),
                Node::local(b_sym),
            ))]),
        )],
        int,
    );
}

/// Checks arrow function with empty return
#[test]
fn check_arrow_epty_return() {
    let mut int = Interner::new();
    check_parser(
        "(a, b) => { return; }",
        &[Node::arrow_function_decl(
            vec![
                FormalParameter::new(int.get_or_intern("a"), None, false),
                FormalParameter::new(int.get_or_intern("b"), None, false),
            ],
            Node::StatementList(vec![Node::Return(None)]),
        )],
        int,
    );
}

/// Checks an arrow function with empty return, with automatic semicolon insertion.
#[test]
fn check_arrow_empty_return_semicolon_insertion() {
    let mut int = Interner::new();
    check_parser(
        "(a, b) => { return }",
        &[Node::arrow_function_decl(
            vec![
                FormalParameter::new(int.get_or_intern("a"), None, false),
                FormalParameter::new(int.get_or_intern("b"), None, false),
            ],
            Node::StatementList(vec![Node::Return(None)]),
        )],
        int,
    );
}
