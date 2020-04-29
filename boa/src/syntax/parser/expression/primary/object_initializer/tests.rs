use crate::{
    syntax::{
        ast::node::{FormalParameter, MethodDefinitionKind, Node, PropertyDefinition},
        parser::tests::check_parser,
    },
    Interner,
};

/// Checks object literal parsing.
#[test]
fn check_object_literal() {
    let mut int = Interner::new();
    let object_properties = vec![
        PropertyDefinition::property(int.get_or_intern("a"), Node::const_node(true)),
        PropertyDefinition::property(int.get_or_intern("b"), Node::const_node(false)),
    ];

    check_parser(
        "const x = {
            a: true,
            b: false,
        };
        ",
        &[Node::const_decl(vec![(
            int.get_or_intern("x"),
            Node::Object(object_properties),
        )])],
        int,
    );
}

/// Tests short function syntax.
#[test]
fn check_object_short_function() {
    let mut int = Interner::new();
    let object_properties = vec![
        PropertyDefinition::property(int.get_or_intern("a"), Node::const_node(true)),
        PropertyDefinition::method_definition(
            MethodDefinitionKind::Ordinary,
            int.get_or_intern("b"),
            Node::function_decl::<_, String, _, _>(None, Vec::new(), Node::StatementList(vec![])),
        ),
    ];

    check_parser(
        "const x = {
            a: true,
            b() {},
        };
        ",
        &[Node::ConstDecl(vec![(
            int.get_or_intern("x"),
            Node::Object(object_properties),
        )])],
        int,
    );
}

/// Testing short function syntax with arguments.
#[test]
fn check_object_short_function_arguments() {
    let mut int = Interner::new();
    let object_properties = vec![
        PropertyDefinition::property(int.get_or_intern("a"), Node::const_node(true)),
        PropertyDefinition::method_definition(
            MethodDefinitionKind::Ordinary,
            int.get_or_intern("b"),
            Node::function_decl(
                None,
                vec![FormalParameter::new(int.get_or_intern("test"), None, false)],
                Node::StatementList(Vec::new()),
            ),
        ),
    ];

    check_parser(
        "const x = {
            a: true,
            b(test) {}
         };
        ",
        &[Node::ConstDecl(vec![(
            int.get_or_intern("x"),
            Node::Object(object_properties),
        )])],
        int,
    );
}

#[test]
fn check_object_getter() {
    let mut int = Interner::new();
    let object_properties = vec![
        PropertyDefinition::property(int.get_or_intern("a"), Node::const_node(true)),
        PropertyDefinition::method_definition(
            MethodDefinitionKind::Get,
            int.get_or_intern("b"),
            Node::FunctionDecl(None, Vec::new(), Box::new(Node::StatementList(Vec::new()))),
        ),
    ];

    check_parser(
        "const x = {
            a: true,
            get b() {}
        };
        ",
        &[Node::ConstDecl(vec![(
            int.get_or_intern("x"),
            Node::Object(object_properties),
        )])],
        int,
    );
}

#[test]
fn check_object_setter() {
    let mut int = Interner::new();
    let object_properties = vec![
        PropertyDefinition::property(int.get_or_intern("a"), Node::const_node(true)),
        PropertyDefinition::method_definition(
            MethodDefinitionKind::Set,
            int.get_or_intern("b"),
            Node::FunctionDecl(
                None,
                vec![FormalParameter::new(int.get_or_intern("test"), None, false)],
                Box::new(Node::StatementList(Vec::new())),
            ),
        ),
    ];

    check_parser(
        "const x = {
            a: true,
            set b(test) {}
        };
        ",
        &[Node::ConstDecl(vec![(
            int.get_or_intern("x"),
            Node::Object(object_properties),
        )])],
        int,
    );
}
