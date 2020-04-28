use crate::{
    syntax::{ast::node::Node, parser::tests::check_parser},
    Interner,
};

#[test]
fn check_string() {
    // Check empty string
    check_parser("\"\"", &[Node::const_node("")]);

    // Check non-empty string
    check_parser("\"hello\"", &[Node::const_node("hello")]);
}
