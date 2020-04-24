use crate::{
    syntax::ast::{
        constant::Const,
        op::{BinOp, Operator, UnaryOp},
    },
    Interner, InternerSym,
};
use gc_derive::{Finalize, Trace};
use std::fmt;

#[cfg(feature = "serde-ast")]
use serde::{Deserialize, Serialize};

/// A Javascript AST Node.
#[cfg_attr(feature = "serde-ast", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, Trace, Finalize, PartialEq)]
pub enum Node {
    /// Create an array with items inside.
    ArrayDecl(Vec<Node>),
    /// Create an arrow function with the given arguments and internal AST node.
    ArrowFunctionDecl(Vec<FormalParameter>, Box<Node>),
    /// Assign an AST node result to an AST node.
    Assign(Box<Node>, Box<Node>),
    /// Run an operation between 2 AST nodes.
    BinOp(BinOp, Box<Node>, Box<Node>),
    /// Run several AST nodes from top-to-bottom.
    Block(Vec<Node>),
    /// Break statement with an optional label.
    Break(Option<InternerSym>),
    /// Call a function with some values.
    Call(Box<Node>, Vec<Node>),
    /// Conditional Operator (`{condition} ? {if true} : {if false}`).
    ConditionalOp(Box<Node>, Box<Node>, Box<Node>),
    /// Make a constant value.
    Const(Const),
    /// Const declaration.
    ConstDecl(Vec<(InternerSym, Node)>),
    /// Continue with an optional label.
    Continue(Option<InternerSym>),
    /// do [body] while [cond]
    DoWhileLoop(Box<Node>, Box<Node>),
    /// Create a function with the given name, arguments, and internal AST node.
    FunctionDecl(Option<InternerSym>, Vec<FormalParameter>, Box<Node>),
    /// Gets the constant field of a value.
    GetConstField(Box<Node>, InternerSym),
    /// Gets the [field] of a value.
    GetField(Box<Node>, Box<Node>),
    /// [init], [cond], [step], body
    ForLoop(
        Option<Box<Node>>,
        Option<Box<Node>>,
        Option<Box<Node>>,
        Box<Node>,
    ),
    /// Check if a conditional expression is true and run an expression if it is and another expression if it isn't
    If(Box<Node>, Box<Node>, Option<Box<Node>>),
    /// Let declaraton
    LetDecl(Vec<(InternerSym, Option<Node>)>),
    /// Load a reference to a value, or a function argument
    Local(InternerSym),
    /// New
    New(Box<Node>),
    /// Object Declaration
    Object(Vec<PropertyDefinition>),
    /// Return the expression from a function
    Return(Option<Box<Node>>),
    /// Run blocks whose cases match the expression
    Switch(Box<Node>, Vec<(Node, Vec<Node>)>, Option<Box<Node>>),
    /// `...a` - spread an iterable value
    Spread(Box<Node>),
    // Similar to Block but without the braces
    StatementList(Vec<Node>),
    /// Throw a value
    Throw(Box<Node>),
    /// Return a string representing the type of the given expression
    TypeOf(Box<Node>),
    /// Try / Catch
    Try(
        Box<Node>,
        Option<Box<Node>>,
        Option<Box<Node>>,
        Option<Box<Node>>,
    ),
    /// The JavaScript `this` keyword refers to the object it belongs to.
    ///
    /// A property of an execution context (global, function or eval) that,
    /// in non–strict mode, is always a reference to an object and in strict
    /// mode can be any value.
    ///
    /// For more information, please check: <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/this>
    This,
    /// Run an operation on a value
    UnaryOp(UnaryOp, Box<Node>),
    /// A variable declaration
    VarDecl(Vec<(InternerSym, Option<Node>)>),
    /// Repeatedly run an expression while the conditional expression resolves to true
    WhileLoop(Box<Node>, Box<Node>),
}

impl Operator for Node {
    fn get_assoc(&self) -> bool {
        match *self {
            Self::UnaryOp(_, _) | Self::TypeOf(_) | Self::If(_, _, _) | Self::Assign(_, _) => false,
            _ => true,
        }
    }
    fn get_precedence(&self) -> u64 {
        match self {
            Self::GetField(_, _) | Self::GetConstField(_, _) => 1,
            Self::Call(_, _) => 2,
            Self::UnaryOp(UnaryOp::IncrementPost, _)
            | Self::UnaryOp(UnaryOp::IncrementPre, _)
            | Self::UnaryOp(UnaryOp::DecrementPost, _)
            | Self::UnaryOp(UnaryOp::DecrementPre, _) => 3,
            Self::UnaryOp(UnaryOp::Not, _)
            | Self::UnaryOp(UnaryOp::Tilde, _)
            | Self::UnaryOp(UnaryOp::Minus, _)
            | Self::TypeOf(_) => 4,
            Self::BinOp(op, _, _) => op.get_precedence(),
            Self::If(_, _, _) => 15,
            // 16 should be yield
            Self::Assign(_, _) => 17,
            _ => 19,
        }
    }
}

impl Node {
    /// Creates an `ArrayDecl` AST node.
    pub fn array_decl<N>(nodes: N) -> Self
    where
        N: Into<Vec<Node>>,
    {
        Self::ArrayDecl(nodes.into())
    }

    /// Creates an `ArraowFunctionDecl` AST node.
    pub fn arrow_function_decl<P, B>(params: P, body: B) -> Self
    where
        P: Into<Vec<FormalParameter>>,
        B: Into<Box<Node>>,
    {
        Self::ArrowFunctionDecl(params.into(), body.into())
    }

    /// Creates an `Assign` AST node.
    pub fn assign<L, R>(lhs: L, rhs: R) -> Self
    where
        L: Into<Box<Node>>,
        R: Into<Box<Node>>,
    {
        Self::Assign(lhs.into(), rhs.into())
    }

    /// Creates a `BinOp` AST node.
    pub fn bin_op<O, L, R>(op: O, lhs: L, rhs: R) -> Self
    where
        O: Into<BinOp>,
        L: Into<Box<Node>>,
        R: Into<Box<Node>>,
    {
        Self::BinOp(op.into(), lhs.into(), rhs.into())
    }

    /// Creates a `Block` AST node.
    pub fn block<N>(nodes: N) -> Self
    where
        N: Into<Vec<Node>>,
    {
        Self::Block(nodes.into())
    }

    /// Creates a `Break` AST node.
    pub fn break_node<OL>(label: OL) -> Self
    where
        OL: Into<Option<InternerSym>>,
    {
        Self::Break(label.into())
    }

    /// Creates a `Call` AST node.
    pub fn call<F, P>(function: F, params: P) -> Self
    where
        F: Into<Box<Node>>,
        P: Into<Vec<Node>>,
    {
        Self::Call(function.into(), params.into())
    }

    /// Creates a `ConditionalOp` AST node.
    pub fn conditional_op<C, T, F>(condition: C, if_true: T, if_false: F) -> Self
    where
        C: Into<Box<Node>>,
        T: Into<Box<Node>>,
        F: Into<Box<Node>>,
    {
        Self::ConditionalOp(condition.into(), if_true.into(), if_false.into())
    }

    /// Creates a `Const` AST node.
    pub fn const_node<C>(node: C) -> Self
    where
        C: Into<Const>,
    {
        Self::Const(node.into())
    }

    /// Creates a `ConstDecl` AST node.
    pub fn const_decl<D>(decl: D) -> Self
    where
        D: Into<Vec<(InternerSym, Node)>>,
    {
        Self::ConstDecl(decl.into())
    }

    /// Creates a `Continue` AST node.
    pub fn continue_node<OL>(label: OL) -> Self
    where
        OL: Into<Option<InternerSym>>,
    {
        Self::Continue(label.into())
    }

    /// Creates a `DoWhileLoop` AST node.
    pub fn do_while_loop<B, C>(body: B, condition: C) -> Self
    where
        B: Into<Box<Node>>,
        C: Into<Box<Node>>,
    {
        Self::DoWhileLoop(body.into(), condition.into())
    }

    /// Creates a `FunctionDecl` AST node.
    pub fn function_decl<ON, P, B>(name: ON, params: P, body: B) -> Self
    where
        ON: Into<Option<InternerSym>>,
        P: Into<Vec<FormalParameter>>,
        B: Into<Box<Node>>,
    {
        Self::FunctionDecl(name.into(), params.into(), body.into())
    }

    /// Creates a `GetConstField` AST node.
    pub fn get_const_field<V>(value: V, label: InternerSym) -> Self
    where
        V: Into<Box<Node>>,
    {
        Self::GetConstField(value.into(), label)
    }

    /// Creates a `GetField` AST node.
    pub fn get_field<V, F>(value: V, field: F) -> Self
    where
        V: Into<Box<Node>>,
        F: Into<Box<Node>>,
    {
        Self::GetField(value.into(), field.into())
    }

    /// Creates a `ForLoop` AST node.
    pub fn for_loop<OI, OC, OS, I, C, S, B>(init: OI, condition: OC, step: OS, body: B) -> Self
    where
        OI: Into<Option<I>>,
        OC: Into<Option<C>>,
        OS: Into<Option<S>>,
        I: Into<Box<Node>>,
        C: Into<Box<Node>>,
        S: Into<Box<Node>>,
        B: Into<Box<Node>>,
    {
        Self::ForLoop(
            init.into().map(I::into),
            condition.into().map(C::into),
            step.into().map(S::into),
            body.into(),
        )
    }

    /// Creates an `If` AST node.
    pub fn if_node<C, B, E, OE>(condition: C, body: B, else_node: OE) -> Self
    where
        C: Into<Box<Node>>,
        B: Into<Box<Node>>,
        E: Into<Box<Node>>,
        OE: Into<Option<E>>,
    {
        Self::If(condition.into(), body.into(), else_node.into().map(E::into))
    }

    /// Creates a `LetDecl` AST node.
    pub fn let_decl<I>(init: I) -> Self
    where
        I: Into<Vec<(InternerSym, Option<Node>)>>,
    {
        Self::LetDecl(init.into())
    }

    /// Creates a `Local` AST node.
    pub fn local(name: InternerSym) -> Self {
        Self::Local(name)
    }

    /// Creates a `New` AST node.
    pub fn new<N>(node: N) -> Self
    where
        N: Into<Box<Node>>,
    {
        Self::New(node.into())
    }

    /// Creates an `Object` AST node.
    pub fn object<D>(def: D) -> Self
    where
        D: Into<Vec<PropertyDefinition>>,
    {
        Self::Object(def.into())
    }

    /// Creates a `Return` AST node.
    pub fn return_node<E, OE>(expr: OE) -> Self
    where
        E: Into<Box<Node>>,
        OE: Into<Option<E>>,
    {
        Self::Return(expr.into().map(E::into))
    }

    /// Creates a `Switch` AST node.
    pub fn switch<V, C, OD, D>(val: V, cases: C, default: OD) -> Self
    where
        V: Into<Box<Node>>,
        C: Into<Vec<(Node, Vec<Node>)>>,
        OD: Into<Option<D>>,
        D: Into<Box<Node>>,
    {
        Self::Switch(val.into(), cases.into(), default.into().map(D::into))
    }

    /// Creates a `Spread` AST node.
    pub fn spread<V>(val: V) -> Self
    where
        V: Into<Box<Node>>,
    {
        Self::Spread(val.into())
    }

    /// Creates a `StatementList` AST node.
    pub fn statement_list<L>(list: L) -> Self
    where
        L: Into<Vec<Node>>,
    {
        Self::StatementList(list.into())
    }

    /// Creates a `Throw` AST node.
    pub fn throw<V>(val: V) -> Self
    where
        V: Into<Box<Node>>,
    {
        Self::Throw(val.into())
    }

    /// Creates a `TypeOf` AST node.
    pub fn type_of<E>(expr: E) -> Self
    where
        E: Into<Box<Node>>,
    {
        Self::TypeOf(expr.into())
    }

    /// Creates a `Try` AST node.
    pub fn try_node<T, OC, OP, OF, C, P, F>(try_node: T, catch: OC, param: OP, finally: OF) -> Self
    where
        T: Into<Box<Node>>,
        OC: Into<Option<C>>,
        OP: Into<Option<P>>,
        OF: Into<Option<F>>,
        C: Into<Box<Node>>,
        P: Into<Box<Node>>,
        F: Into<Box<Node>>,
    {
        let catch = catch.into().map(C::into);
        let finally = finally.into().map(F::into);

        debug_assert!(
            catch.is_some() || finally.is_some(),
            "try/catch must have a catch or a finally block"
        );

        Self::Try(try_node.into(), catch, param.into().map(P::into), finally)
    }

    /// Creates a `This` AST node.
    pub fn this() -> Self {
        Self::This
    }

    /// Creates a `UnaryOp` AST node.
    pub fn unary_op<V>(op: UnaryOp, val: V) -> Self
    where
        V: Into<Box<Node>>,
    {
        Self::UnaryOp(op, val.into())
    }

    /// Creates a `VarDecl` AST node.
    pub fn var_decl<I>(init: I) -> Self
    where
        I: Into<Vec<(InternerSym, Option<Node>)>>,
    {
        Self::VarDecl(init.into())
    }

    /// Creates a `WhileLoop` AST node.
    pub fn while_loop<C, B>(condition: C, body: B) -> Self
    where
        C: Into<Box<Node>>,
        B: Into<Box<Node>>,
    {
        Self::WhileLoop(condition.into(), body.into())
    }

    /// Creates an object with the `fmt::Display` implementation for this node.
    pub fn display<'f>(&'f self, interner: &'f Interner) -> NodeDisplay<'f, 'f> {
        NodeDisplay {
            node: &self,
            interner,
        }
    }
}

/// Structure implementing the `fmt::Display` trait for a `Node`.
#[derive(Debug)]
pub struct NodeDisplay<'n, 'i> {
    node: &'n Node,
    interner: &'i Interner,
}

impl NodeDisplay<'_, '_> {
    /// Implements the display formatting with indentation.
    fn display(&self, f: &mut fmt::Formatter<'_>, indentation: usize) -> fmt::Result {
        let indent = "    ".repeat(indentation);
        match *self.node {
            Node::Block(_) => {}
            _ => write!(f, "{}", indent)?,
        }

        match *self.node {
            Node::Const(ref c) => write!(f, "{}", c.display(self.interner)),
            Node::ConditionalOp(ref cond, ref if_true, ref if_false) => write!(
                f,
                "{} ? {} : {}",
                cond.display(self.interner),
                if_true.display(self.interner),
                if_false.display(self.interner)
            ),
            Node::ForLoop(_, _, _, _) => write!(f, "for loop"), // TODO
            Node::This => write!(f, "this"),
            Node::Try(_, _, _, _) => write!(f, "try/catch/finally"), // TODO
            Node::Break(l) => write!(
                f,
                "break{}",
                if let Some(label) = l {
                    format!(
                        " {}",
                        self.interner
                            .resolve(label)
                            .expect("could not find label string for break statement")
                    )
                } else {
                    String::new()
                }
            ),
            Node::Continue(l) => write!(
                f,
                "continue{}",
                if let Some(label) = l {
                    format!(
                        " {}",
                        self.interner
                            .resolve(label)
                            .expect("could not find label string for continue statement")
                    )
                } else {
                    String::new()
                }
            ),
            Node::Spread(ref node) => write!(f, "...{}", node.display(self.interner)),
            Node::Block(ref block) => {
                writeln!(f, "{{")?;
                for node in block.iter() {
                    node.display(self.interner).display(f, indentation + 1)?;

                    match node {
                        Node::Block(_)
                        | Node::If(_, _, _)
                        | Node::Switch(_, _, _)
                        | Node::FunctionDecl(_, _, _)
                        | Node::WhileLoop(_, _)
                        | Node::StatementList(_) => {}
                        _ => write!(f, ";")?,
                    }
                    writeln!(f)?;
                }
                write!(f, "{}}}", indent)
            }
            Node::StatementList(ref list) => {
                for node in list.iter() {
                    node.display(self.interner).display(f, indentation + 1)?;

                    match node {
                        Node::Block(_)
                        | Node::If(_, _, _)
                        | Node::Switch(_, _, _)
                        | Node::FunctionDecl(_, _, _)
                        | Node::WhileLoop(_, _)
                        | Node::StatementList(_) => {}
                        _ => write!(f, ";")?,
                    }
                    writeln!(f)?;
                }
                Ok(())
            }
            Node::Local(s) => write!(
                f,
                "{}",
                self.interner
                    .resolve(s)
                    .expect("could not find local identifier name")
            ),
            Node::GetConstField(ref ex, field) => write!(
                f,
                "{}.{}",
                ex.display(self.interner),
                self.interner
                    .resolve(field)
                    .expect("field name string not found")
            ),
            Node::GetField(ref ex, ref field) => write!(
                f,
                "{}[{}]",
                ex.display(self.interner),
                field.display(self.interner)
            ),
            Node::Call(ref ex, ref args) => {
                write!(f, "{}(", ex.display(self.interner))?;
                let arg_strs: Vec<String> = args
                    .iter()
                    .map(|arg| arg.display(self.interner).to_string())
                    .collect();
                write!(f, "{})", arg_strs.join(", "))
            }
            Node::New(ref call) => {
                let (func, args) = match call.as_ref() {
                    Node::Call(func, args) => (func, args),
                    _ => unreachable!("Node::New(ref call): 'call' must only be Node::Call type."),
                };

                write!(f, "new {}", func.display(self.interner))?;
                f.write_str("(")?;
                let mut first = true;
                for e in args.iter() {
                    if !first {
                        f.write_str(", ")?;
                    }
                    first = false;
                    write!(f, "{}", e.display(self.interner))?;
                }
                f.write_str(")")
            }
            Node::WhileLoop(ref cond, ref node) => {
                write!(f, "while ({}) ", cond.display(self.interner))?;
                node.display(self.interner).display(f, indentation)
            }
            Node::DoWhileLoop(ref node, ref cond) => {
                write!(f, "do")?;
                node.display(self.interner).display(f, indentation)?;
                write!(f, "while ({})", cond.display(self.interner))
            }
            Node::If(ref cond, ref node, None) => {
                write!(f, "if ({}) ", cond.display(self.interner))?;
                node.display(self.interner).display(f, indentation)
            }
            Node::If(ref cond, ref node, Some(ref else_e)) => {
                write!(f, "if ({}) ", cond.display(self.interner))?;
                node.display(self.interner).display(f, indentation)?;
                f.write_str(" else ")?;
                else_e.display(self.interner).display(f, indentation)
            }
            Node::Switch(ref val, ref vals, None) => {
                writeln!(f, "switch ({}) {{", val.display(self.interner))?;
                for e in vals.iter() {
                    writeln!(f, "{}case {}:", indent, e.0.display(self.interner))?;
                    join_nodes(f, &e.1, self.interner)?;
                }
                writeln!(f, "{}}}", indent)
            }
            Node::Switch(ref val, ref vals, Some(ref def)) => {
                writeln!(f, "switch ({}) {{", val.display(self.interner))?;
                for e in vals.iter() {
                    writeln!(f, "{}case {}:", indent, e.0.display(self.interner))?;
                    join_nodes(f, &e.1, self.interner)?;
                }
                writeln!(f, "{}default:", indent)?;
                def.display(self.interner).display(f, indentation + 1)?;
                write!(f, "{}}}", indent)
            }
            Node::Object(ref properties) => {
                f.write_str("{\n")?;
                for property in properties {
                    match property {
                        PropertyDefinition::IdentifierReference(key) => {
                            write!(
                                f,
                                "{}    {},",
                                indent,
                                self.interner
                                    .resolve(*key)
                                    .expect("could not find identifier reference key string")
                            )?;
                        }
                        PropertyDefinition::Property(key, value) => {
                            write!(
                                f,
                                "{}    {}: {},",
                                indent,
                                self.interner
                                    .resolve(*key)
                                    .expect("could not find property key string"),
                                value.display(self.interner)
                            )?;
                        }
                        PropertyDefinition::SpreadObject(key) => {
                            write!(f, "{}    ...{},", indent, key.display(self.interner))?;
                        }
                        PropertyDefinition::MethodDefinition(_kind, _key, _node) => {
                            // TODO: Implement display for PropertyDefinition::MethodDefinition.
                            unimplemented!("Display for PropertyDefinition::MethodDefinition");
                        }
                    }
                }
                f.write_str("}")
            }
            Node::ArrayDecl(ref arr) => {
                f.write_str("[")?;
                join_nodes(f, arr, self.interner)?;
                f.write_str("]")
            }
            Node::FunctionDecl(ref name, ref _args, ref node) => {
                write!(f, "function ")?;
                if let Some(func_name) = name {
                    write!(
                        f,
                        "{}",
                        self.interner
                            .resolve(*func_name)
                            .expect("function name string not found")
                    )?;
                }
                write!(f, "{{")?;
                //join_nodes(f, args)?; TODO: port
                f.write_str("} ")?;
                node.display(self.interner).display(f, indentation + 1)
            }
            Node::ArrowFunctionDecl(ref _args, ref node) => {
                write!(f, "(")?;
                //join_nodes(f, args)?; TODO: port
                f.write_str(") => ")?;
                node.display(self.interner).display(f, indentation)
            }
            Node::BinOp(ref op, ref a, ref b) => write!(
                f,
                "{} {} {}",
                a.display(self.interner),
                op,
                b.display(self.interner)
            ),
            Node::UnaryOp(ref op, ref a) => write!(f, "{}{}", op, a.display(self.interner)),
            Node::Return(Some(ref ex)) => write!(f, "return {}", ex.display(self.interner)),
            Node::Return(None) => write!(f, "return"),
            Node::Throw(ref ex) => write!(f, "throw {}", ex.display(self.interner)),
            Node::Assign(ref ref_e, ref val) => write!(
                f,
                "{} = {}",
                ref_e.display(self.interner),
                val.display(self.interner)
            ),
            Node::VarDecl(ref vars) | Node::LetDecl(ref vars) => {
                if let Node::VarDecl(_) = *self.node {
                    f.write_str("var ")?;
                } else {
                    f.write_str("let ")?;
                }
                for (key, val) in vars.iter() {
                    let key_str = self.interner.resolve(*key).expect("key string disappeared");
                    match val {
                        Some(x) => write!(f, "{} = {}", key_str, x.display(self.interner))?,
                        None => write!(f, "{}", key_str)?,
                    }
                }
                Ok(())
            }
            Node::ConstDecl(ref vars) => {
                f.write_str("const ")?;
                for (key, val) in vars.iter() {
                    write!(
                        f,
                        "{} = {}",
                        self.interner
                            .resolve(*key)
                            .expect("could not find const key string"),
                        val.display(self.interner)
                    )?
                }
                Ok(())
            }
            Node::TypeOf(ref e) => write!(f, "typeof {}", e.display(self.interner)),
        }
    }
}

impl fmt::Display for NodeDisplay<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.display(f, 0)
    }
}

/// Utility to join multiple Nodes into a single string.
fn join_nodes(f: &mut fmt::Formatter<'_>, nodes: &[Node], interner: &Interner) -> fmt::Result {
    let mut first = true;
    for e in nodes {
        if !first {
            f.write_str(", ")?;
        }
        first = false;
        write!(f, "{}", e.display(interner))?;
    }
    Ok(())
}

/// "Formal parameter" is a fancy way of saying "function parameter".
///
/// In the declaration of a function, the parameters must be identifiers,
/// not any value like numbers, strings, or objects.
///```javascript
///function foo(formalParametar1, formalParametar2) {
///}
///```
/// For more information, please check <https://tc39.es/ecma262/#prod-FormalParameter>
#[cfg_attr(feature = "serde-ast", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, PartialEq, Trace, Finalize)]
pub struct FormalParameter {
    pub name: InternerSym,
    pub init: Option<Box<Node>>,
    pub is_rest_param: bool,
}

impl FormalParameter {
    pub fn new(name: InternerSym, init: Option<Box<Node>>, is_rest_param: bool) -> Self {
        Self {
            name,
            init,
            is_rest_param,
        }
    }
}

// TODO: Support all features: https://tc39.es/ecma262/#prod-PropertyDefinition
#[cfg_attr(feature = "serde-ast", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, PartialEq, Trace, Finalize)]
pub enum PropertyDefinition {
    IdentifierReference(InternerSym),
    Property(InternerSym, Node),
    MethodDefinition(MethodDefinitionKind, InternerSym, Node),
    SpreadObject(Node),
}

impl PropertyDefinition {
    /// Creates an `IdentifierReference` property definition.
    pub fn identifier_reference(ident: InternerSym) -> Self {
        Self::IdentifierReference(ident)
    }

    /// Creates a `Property` definition.
    pub fn property<V>(name: InternerSym, value: V) -> Self
    where
        V: Into<Node>,
    {
        Self::Property(name, value.into())
    }

    /// Creates a `MethodDefinition`.
    pub fn method_definition<B>(kind: MethodDefinitionKind, name: InternerSym, body: B) -> Self
    where
        B: Into<Node>,
    {
        Self::MethodDefinition(kind, name, body.into())
    }

    /// Creates a `SpreadObject`.
    pub fn spread_object<O>(obj: O) -> Self
    where
        O: Into<Node>,
    {
        Self::SpreadObject(obj.into())
    }
}

#[cfg_attr(feature = "serde-ast", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, PartialEq, Trace, Finalize)]
pub enum MethodDefinitionKind {
    Get,
    Set,
    Ordinary,
}
