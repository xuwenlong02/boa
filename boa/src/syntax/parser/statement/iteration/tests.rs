use crate::{
    syntax::{
        ast::node::Node,
        ast::op::{AssignOp, BinOp, CompOp, UnaryOp},
        parser::tests::check_parser,
    },
    Interner,
};

/// Checks do-while statement parsing.
#[test]
fn check_do_while() {
    let mut int = Interner::new();
    check_parser(
        r#"do {
            a += 1;
        } while (true)"#,
        &[Node::do_while_loop(
            Node::Block(vec![Node::bin_op(
                BinOp::Assign(AssignOp::Add),
                Node::local(int.get_or_intern("a")),
                Node::const_node(1.0),
            )]),
            Node::const_node(true),
        )],
        int,
    );
}

// Checks automatic semicolon insertion after do-while.
#[test]
fn check_do_while_semicolon_insertion() {
    let mut int = Interner::new();
    let i_sym = int.get_or_intern("i");
    let console_sym = int.get_or_intern("console");
    let log_sym = int.get_or_intern("log");

    check_parser(
        r#"var i = 0;
        do {console.log("hello");} while(i++ < 10) console.log("end");"#,
        &[
            Node::VarDecl(vec![(i_sym, Some(Node::const_node(0.0)))]),
            Node::do_while_loop(
                Node::Block(vec![Node::call(
                    Node::get_const_field(Node::local(console_sym), log_sym),
                    vec![Node::const_node(int.get_or_intern("hello"))],
                )]),
                Node::bin_op(
                    BinOp::Comp(CompOp::LessThan),
                    Node::unary_op(UnaryOp::IncrementPost, Node::local(i_sym)),
                    Node::const_node(10.0),
                ),
            ),
            Node::call(
                Node::get_const_field(Node::local(console_sym), log_sym),
                vec![Node::const_node(int.get_or_intern("end"))],
            ),
        ],
        int,
    );
}
