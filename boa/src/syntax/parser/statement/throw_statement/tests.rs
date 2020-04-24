use crate::{
    syntax::{
        ast::node::Node,
        parser::tests::{check_invalid, check_parser},
    },
    Interner,
};

#[test]
fn check_throw_parsing() {
    let mut int = Interner::new();
    check_parser(
        "throw 'error';",
        &[Node::throw(Node::const_node(int.get_or_intern("error")))],
        int,
    );
}

#[test]
fn check_invalid_throw_parsing() {
    let mut int = Interner::new();
    check_invalid("throw;");
}
