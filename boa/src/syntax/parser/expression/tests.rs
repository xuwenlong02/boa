use crate::{
    syntax::{
        ast::node::Node,
        ast::op::{AssignOp, BinOp, BitOp, NumOp},
        parser::tests::check_parser,
    },
    Interner,
};

/// Checks numeric operations
#[test]
fn check_numeric_operations() {
    let mut int = Interner::new();
    check_parser(
        "a + b",
        &[Node::bin_op(
            NumOp::Add,
            Node::local(int.get_or_intern("a")),
            Node::local(int.get_or_intern("b")),
        )],
        int,
    );

    let mut int = Interner::new();
    check_parser(
        "a+1",
        &[Node::bin_op(
            NumOp::Add,
            Node::local(int.get_or_intern("a")),
            Node::const_node(1.0),
        )],
        int,
    );

    let mut int = Interner::new();
    check_parser(
        "a - b",
        &[Node::bin_op(
            NumOp::Sub,
            Node::local(int.get_or_intern("a")),
            Node::local(int.get_or_intern("b")),
        )],
        int,
    );

    let mut int = Interner::new();
    check_parser(
        "a-1",
        &[Node::bin_op(
            NumOp::Sub,
            Node::local(int.get_or_intern("a")),
            Node::const_node(1.0),
        )],
        int,
    );

    let mut int = Interner::new();
    check_parser(
        "a / b",
        &[Node::bin_op(
            NumOp::Div,
            Node::local(int.get_or_intern("a")),
            Node::local(int.get_or_intern("b")),
        )],
        int,
    );

    let mut int = Interner::new();
    check_parser(
        "a/2",
        &[Node::bin_op(
            NumOp::Div,
            Node::local(int.get_or_intern("a")),
            Node::const_node(2.0),
        )],
        int,
    );

    let mut int = Interner::new();
    check_parser(
        "a * b",
        &[Node::bin_op(
            NumOp::Mul,
            Node::local(int.get_or_intern("a")),
            Node::local(int.get_or_intern("b")),
        )],
        int,
    );

    let mut int = Interner::new();
    check_parser(
        "a*2",
        &[Node::bin_op(
            NumOp::Mul,
            Node::local(int.get_or_intern("a")),
            Node::const_node(2.0),
        )],
        int,
    );

    let mut int = Interner::new();
    check_parser(
        "a ** b",
        &[Node::bin_op(
            NumOp::Exp,
            Node::local(int.get_or_intern("a")),
            Node::local(int.get_or_intern("b")),
        )],
        int,
    );

    let mut int = Interner::new();
    check_parser(
        "a**2",
        &[Node::bin_op(
            NumOp::Exp,
            Node::local(int.get_or_intern("a")),
            Node::const_node(2.0),
        )],
        int,
    );

    let mut int = Interner::new();
    check_parser(
        "a % b",
        &[Node::bin_op(
            NumOp::Mod,
            Node::local(int.get_or_intern("a")),
            Node::local(int.get_or_intern("b")),
        )],
        int,
    );

    let mut int = Interner::new();
    check_parser(
        "a%2",
        &[Node::bin_op(
            NumOp::Mod,
            Node::local(int.get_or_intern("a")),
            Node::const_node(2.0),
        )],
        int,
    );
}

// Checks complex numeric operations.
#[test]
fn check_complex_numeric_operations() {
    let mut int = Interner::new();
    check_parser(
        "a + d*(b-3)+1",
        &[Node::bin_op(
            NumOp::Add,
            Node::bin_op(
                NumOp::Add,
                Node::local(int.get_or_intern("a")),
                Node::bin_op(
                    NumOp::Mul,
                    Node::local(int.get_or_intern("d")),
                    Node::bin_op(
                        NumOp::Sub,
                        Node::local(int.get_or_intern("b")),
                        Node::const_node(3.0),
                    ),
                ),
            ),
            Node::const_node(1.0),
        )],
        int,
    );
}

/// Checks bitwise operations.
#[test]
fn check_bitwise_operations() {
    let mut int = Interner::new();
    check_parser(
        "a & b",
        &[Node::bin_op(
            BinOp::Bit(BitOp::And),
            Node::local(int.get_or_intern("a")),
            Node::local(int.get_or_intern("b")),
        )],
        int,
    );

    let mut int = Interner::new();
    check_parser(
        "a&b",
        &[Node::bin_op(
            BinOp::Bit(BitOp::And),
            Node::local(int.get_or_intern("a")),
            Node::local(int.get_or_intern("b")),
        )],
        int,
    );

    let mut int = Interner::new();
    check_parser(
        "a | b",
        &[Node::bin_op(
            BinOp::Bit(BitOp::Or),
            Node::local(int.get_or_intern("a")),
            Node::local(int.get_or_intern("b")),
        )],
        int,
    );

    let mut int = Interner::new();
    check_parser(
        "a|b",
        &[Node::bin_op(
            BinOp::Bit(BitOp::Or),
            Node::local(int.get_or_intern("a")),
            Node::local(int.get_or_intern("b")),
        )],
        int,
    );

    let mut int = Interner::new();
    check_parser(
        "a ^ b",
        &[Node::bin_op(
            BinOp::Bit(BitOp::Xor),
            Node::local(int.get_or_intern("a")),
            Node::local(int.get_or_intern("b")),
        )],
        int,
    );

    let mut int = Interner::new();
    check_parser(
        "a^b",
        &[Node::bin_op(
            BinOp::Bit(BitOp::Xor),
            Node::local(int.get_or_intern("a")),
            Node::local(int.get_or_intern("b")),
        )],
        int,
    );

    check_parser(
        "a << b",
        &[Node::bin_op(
            BinOp::Bit(BitOp::Shl),
            Node::local(int.get_or_intern("a")),
            Node::local(int.get_or_intern("b")),
        )],
        int,
    );

    let mut int = Interner::new();
    check_parser(
        "a<<b",
        &[Node::bin_op(
            BinOp::Bit(BitOp::Shl),
            Node::local(int.get_or_intern("a")),
            Node::local(int.get_or_intern("b")),
        )],
        int,
    );

    let mut int = Interner::new();
    check_parser(
        "a >> b",
        &[Node::bin_op(
            BinOp::Bit(BitOp::Shr),
            Node::local(int.get_or_intern("a")),
            Node::local(int.get_or_intern("b")),
        )],
        int,
    );

    let mut int = Interner::new();
    check_parser(
        "a>>b",
        &[Node::bin_op(
            BinOp::Bit(BitOp::Shr),
            Node::local(int.get_or_intern("a")),
            Node::local(int.get_or_intern("b")),
        )],
        int,
    );
}

/// Checks assignment operations.
#[test]
fn check_assign_operations() {
    let mut int = Interner::new();
    check_parser(
        "a += b",
        &[Node::bin_op(
            BinOp::Assign(AssignOp::Add),
            Node::local(int.get_or_intern("a")),
            Node::local(int.get_or_intern("b")),
        )],
        int,
    );

    let mut int = Interner::new();
    check_parser(
        "a -= b",
        &[Node::bin_op(
            BinOp::Assign(AssignOp::Sub),
            Node::local(int.get_or_intern("a")),
            Node::local(int.get_or_intern("b")),
        )],
        int,
    );

    let mut int = Interner::new();
    check_parser(
        "a *= b",
        &[Node::bin_op(
            BinOp::Assign(AssignOp::Mul),
            Node::local(int.get_or_intern("a")),
            Node::local(int.get_or_intern("b")),
        )],
        int,
    );

    let mut int = Interner::new();
    check_parser(
        "a **= b",
        &[Node::bin_op(
            BinOp::Assign(AssignOp::Exp),
            Node::local(int.get_or_intern("a")),
            Node::local(int.get_or_intern("b")),
        )],
        int,
    );

    let mut int = Interner::new();
    check_parser(
        "a /= b",
        &[Node::bin_op(
            BinOp::Assign(AssignOp::Div),
            Node::local(int.get_or_intern("a")),
            Node::local(int.get_or_intern("b")),
        )],
        int,
    );

    let mut int = Interner::new();
    check_parser(
        "a %= b",
        &[Node::bin_op(
            BinOp::Assign(AssignOp::Mod),
            Node::local(int.get_or_intern("a")),
            Node::local(int.get_or_intern("b")),
        )],
        int,
    );

    let mut int = Interner::new();
    check_parser(
        "a &= b",
        &[Node::bin_op(
            BinOp::Assign(AssignOp::And),
            Node::local(int.get_or_intern("a")),
            Node::local(int.get_or_intern("b")),
        )],
        int,
    );

    let mut int = Interner::new();
    check_parser(
        "a |= b",
        &[Node::bin_op(
            BinOp::Assign(AssignOp::Or),
            Node::local(int.get_or_intern("a")),
            Node::local(int.get_or_intern("b")),
        )],
        int,
    );

    let mut int = Interner::new();
    check_parser(
        "a ^= b",
        &[Node::bin_op(
            BinOp::Assign(AssignOp::Xor),
            Node::local(int.get_or_intern("a")),
            Node::local(int.get_or_intern("b")),
        )],
        int,
    );

    let mut int = Interner::new();
    check_parser(
        "a <<= b",
        &[Node::bin_op(
            BinOp::Assign(AssignOp::Shl),
            Node::local(int.get_or_intern("a")),
            Node::local(int.get_or_intern("b")),
        )],
        int,
    );

    let mut int = Interner::new();
    check_parser(
        "a >>= b",
        &[Node::bin_op(
            BinOp::Assign(AssignOp::Shr),
            Node::local(int.get_or_intern("a")),
            Node::local(int.get_or_intern("b")),
        )],
        int,
    );

    let mut int = Interner::new();
    check_parser(
        "a %= 10 / 2",
        &[Node::bin_op(
            BinOp::Assign(AssignOp::Mod),
            Node::local(int.get_or_intern("a")),
            Node::bin_op(NumOp::Div, Node::const_node(10.0), Node::const_node(2.0)),
        )],
        int,
    );
}
