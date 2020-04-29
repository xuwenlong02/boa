use crate::{
    syntax::{ast::node::Node, parser::tests::check_parser},
    Interner,
};

#[test]
fn check_string() {
    // Check empty string
    let mut int = Interner::new();
    check_parser("\"\"", &[Node::const_node(int.get_or_intern(""))], int);

    // Check non-empty string
    let mut int = Interner::new();
    check_parser(
        "\"hello\"",
        &[Node::const_node(int.get_or_intern("hello"))],
        int,
    );
}
