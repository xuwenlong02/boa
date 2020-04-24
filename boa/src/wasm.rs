use crate::{
    exec::{Executor, Interpreter},
    realm::Realm,
    syntax::{ast::node::Node, lexer::Lexer, parser::Parser},
    Interner,
};
use wasm_bindgen::prelude::*;

// WASM
#[wasm_bindgen]
extern "C" {
    // Use `js_namespace` here to bind `console.log(..)` instead of just
    // `log(..)`
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

#[wasm_bindgen]
pub fn evaluate(src: &str) -> String {
    let mut lexer = Lexer::new(&src);
    match lexer.lex() {
        Ok(_v) => (),
        Err(v) => log(&v.to_string()),
    }

    let tokens = lexer.tokens;

    // Setup executor
    let node: Node;

    match Parser::new(&tokens, lexer.interner).parse_all() {
        Ok(v) => {
            node = v;
        }
        Err(_v) => {
            log("parsing fail");
            return String::from("parsing failed");
        }
    }
    // Create new Realm
    let mut interner = Interner::new();
    let realm = Realm::create(&mut interner);
    let mut engine: Interpreter = Executor::new(realm, interner);
    let result = engine.run(&node);
    match result {
        Ok(v) => v.to_string(),
        Err(v) => format!("{}: {}", "error", v.to_string()),
    }
}
