use crate::{
    syntax::{ast::node::Node, parser::tests::check_parser},
    Interner,
};

#[test]
fn check_throw_parsing() {
    check_parser("throw 'error';", &[Node::throw(Node::const_node("error"))]);
}
