use crate::{
    syntax::{ast::node::Node, parser::tests::check_parser},
    Interner,
};

#[test]
fn check_string() {
    let mut int = Interner::new();

    // Check empty string
    check_parser("\"\"", &[Node::const_node(int.get_or_intern(""))], int);

    let mut int = Interner::new();
    // Check non-empty string
    check_parser(
        "\"hello\"",
        &[Node::const_node(int.get_or_intern("hello"))],
        int,
    );
}
