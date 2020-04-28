//! Tests for the lexer.
#![allow(clippy::indexing_slicing)]

use super::*;
use crate::{syntax::ast::keyword::Keyword, Interner};

/// Checks if the lexer properly lexes the given code.
// #[track_caller] TODO: waiting for https://github.com/rust-lang/rust/issues/47809#issuecomment-609479553
fn check_lexer<I, K>(code: &str, interner: I, tokens: &[K])
where
    I: Into<Option<Interner>>,
    K: Into<TokenKind> + Copy,
{
    let mut interner = interner.into().unwrap_or_else(Interner::new);
    let mut lexer = Lexer::new(code, &mut interner);
    lexer.lex().expect("failed to lex");

    for (i, (lexer, expected)) in lexer
        .tokens
        .into_iter()
        .map(|tk| tk.kind)
        .zip(tokens.iter().copied().map(K::into))
        .enumerate()
    {
        assert_eq!(lexer, expected, "error in token {}", i);
    }
}

#[test]
fn check_single_line_comment() {
    let s1 = "var
    //This is a comment
    true";

    check_lexer(
        s1,
        None,
        &[
            TokenKind::Keyword(Keyword::Var),
            TokenKind::LineTerminator,
            TokenKind::BooleanLiteral(true),
        ],
    )
}

#[test]
fn check_multi_line_comment() {
    let s = "var /* await 
        break 
        */ x";

    let mut int = Interner::new();
    check_lexer(
        s,
        int,
        &[
            TokenKind::Keyword(Keyword::Var),
            TokenKind::identifier(int.get_or_intern("x")),
        ],
    );
}

#[test]
fn check_string() {
    let mut int = Interner::new();
    check_lexer(
        "'aaa' \"bbb\"",
        int,
        &[
            TokenKind::string_literal(int.get_or_intern("aaa")),
            TokenKind::string_literal(int.get_or_intern("bbb")),
        ],
    );
}

#[test]
fn check_punctuators() {
    // https://tc39.es/ecma262/#sec-punctuators
    let s = "{ ( ) [ ] . ... ; , < > <= >= == != === !== \
             + - * % -- << >> >>> & | ^ ! ~ && || ? : \
             = += -= *= &= **= ++ ** <<= >>= >>>= &= |= ^= =>";

    check_lexer(
        s,
        None,
        &[
            Punctuator::OpenBlock,
            Punctuator::OpenParen,
            Punctuator::CloseParen,
            Punctuator::OpenBracket,
            Punctuator::CloseBracket,
            Punctuator::Dot,
            Punctuator::Spread,
            Punctuator::Semicolon,
            Punctuator::Comma,
            Punctuator::LessThan,
            Punctuator::GreaterThan,
            Punctuator::LessThanOrEq,
            Punctuator::GreaterThanOrEq,
            Punctuator::Eq,
            Punctuator::NotEq,
            Punctuator::StrictEq,
            Punctuator::StrictNotEq,
            Punctuator::Add,
            Punctuator::Sub,
            Punctuator::Mul,
            Punctuator::Mod,
            Punctuator::Dec,
            Punctuator::LeftSh,
            Punctuator::RightSh,
            Punctuator::URightSh,
            Punctuator::And,
            Punctuator::Or,
            Punctuator::Xor,
            Punctuator::Not,
            Punctuator::Neg,
            Punctuator::BoolAnd,
            Punctuator::BoolOr,
            Punctuator::Question,
            Punctuator::Colon,
            Punctuator::Assign,
            Punctuator::AssignAdd,
            Punctuator::AssignSub,
            Punctuator::AssignMul,
            Punctuator::AssignAnd,
            Punctuator::AssignPow,
            Punctuator::Inc,
            Punctuator::Exp,
            Punctuator::AssignLeftSh,
            Punctuator::AssignRightSh,
            Punctuator::AssignURightSh,
            Punctuator::AssignAnd,
            Punctuator::AssignOr,
            Punctuator::AssignXor,
            Punctuator::Arrow,
        ],
    );
}

#[test]
fn check_keywords() {
    // https://tc39.es/ecma262/#sec-keywords
    let s = "await break case catch class const continue debugger default delete \
             do else export extends finally for function if import in instanceof \
             new return super switch this throw try typeof var void while with yield";

    check_lexer(
        s,
        None,
        &[
            Keyword::Await,
            Keyword::Break,
            Keyword::Case,
            Keyword::Catch,
            Keyword::Class,
            Keyword::Const,
            Keyword::Continue,
            Keyword::Debugger,
            Keyword::Default,
            Keyword::Delete,
            Keyword::Do,
            Keyword::Else,
            Keyword::Export,
            Keyword::Extends,
            Keyword::Finally,
            Keyword::For,
            Keyword::Function,
            Keyword::If,
            Keyword::Import,
            Keyword::In,
            Keyword::InstanceOf,
            Keyword::New,
            Keyword::Return,
            Keyword::Super,
            Keyword::Switch,
            Keyword::This,
            Keyword::Throw,
            Keyword::Try,
            Keyword::TypeOf,
            Keyword::Var,
            Keyword::Void,
            Keyword::While,
            Keyword::With,
            Keyword::Yield,
        ],
    );
}

#[test]
fn check_variable_definition_tokens() {
    let mut int = Interner::new();

    check_lexer(
        "let a = 'hello';",
        int,
        &[
            TokenKind::Keyword(Keyword::Let),
            TokenKind::identifier(int.get_or_intern("a")),
            TokenKind::string_literal(int.get_or_intern("hello")),
        ],
    )
}

#[test]
fn check_positions() {
    let s = "console.log(\"hello world\"); // Test";
    // ------123456789
    let mut lexer = Lexer::new(s, &mut Interner::new());
    lexer.lex().expect("failed to lex");
    // The first column is 1 (not zero indexed)
    assert_eq!(lexer.tokens[0].pos.column_number, 1);
    assert_eq!(lexer.tokens[0].pos.line_number, 1);
    // Dot Token starts on column 8
    assert_eq!(lexer.tokens[1].pos.column_number, 8);
    assert_eq!(lexer.tokens[1].pos.line_number, 1);
    // Log Token starts on column 9
    assert_eq!(lexer.tokens[2].pos.column_number, 9);
    assert_eq!(lexer.tokens[2].pos.line_number, 1);
    // Open parenthesis token starts on column 12
    assert_eq!(lexer.tokens[3].pos.column_number, 12);
    assert_eq!(lexer.tokens[3].pos.line_number, 1);
    // String token starts on column 13
    assert_eq!(lexer.tokens[4].pos.column_number, 13);
    assert_eq!(lexer.tokens[4].pos.line_number, 1);
    // Close parenthesis token starts on column 26
    assert_eq!(lexer.tokens[5].pos.column_number, 26);
    assert_eq!(lexer.tokens[5].pos.line_number, 1);
    // Semi Colon token starts on column 27
    assert_eq!(lexer.tokens[6].pos.column_number, 27);
    assert_eq!(lexer.tokens[6].pos.line_number, 1);
}

#[test]
#[ignore]
fn test_two_divisions_in_expression() {
    let s = "    return a !== 0 || 1 / a === 1 / b;";
    let mut lexer = Lexer::new(s, &mut Interner::new());
    lexer.lex().expect("failed to lex");
    // dbg!(&lexer.tokens);

    assert_eq!(lexer.tokens[11].pos.column_number, 37);
    assert_eq!(lexer.tokens[11].pos.line_number, 1);
}

#[test]
fn check_line_numbers() {
    let s = "x\ny\n";

    let mut lexer = Lexer::new(s, &mut Interner::new());
    lexer.lex().expect("failed to lex");

    assert_eq!(lexer.tokens[0].pos.column_number, 1);
    assert_eq!(lexer.tokens[0].pos.line_number, 1);

    assert_eq!(lexer.tokens[1].pos.column_number, 2);
    assert_eq!(lexer.tokens[1].pos.line_number, 1);

    assert_eq!(lexer.tokens[2].pos.column_number, 1);
    assert_eq!(lexer.tokens[2].pos.line_number, 2);

    assert_eq!(lexer.tokens[3].pos.column_number, 2);
    assert_eq!(lexer.tokens[3].pos.line_number, 2);
}

// Increment/Decrement
#[test]
fn check_decrement_advances_lexer_2_places() {
    // Here we want an example of decrementing an integer
    let s = "let a = b--;";
    let mut lexer = Lexer::new(s, &mut Interner::new());
    lexer.lex().expect("failed to lex");
    assert_eq!(lexer.tokens[4].kind, TokenKind::Punctuator(Punctuator::Dec));
    // Decrementing means adding 2 characters '--', the lexer should consume it as a single token
    // and move the curser forward by 2, meaning the next token should be a semicolon
    assert_eq!(
        lexer.tokens[5].kind,
        TokenKind::Punctuator(Punctuator::Semicolon)
    );
}

#[test]
fn numbers() {
    let s = "1 2 0x34 056 7.89 42. 5e3 5e+3 5e-3 0b10 0O123 0999 1.0e1 1.0e-1 1.0E1 1E1 0.0 0.12";

    check_lexer(
        s,
        None,
        &[
            TokenKind::NumericLiteral(1.0),
            TokenKind::NumericLiteral(2.0),
            TokenKind::NumericLiteral(52.0),
            TokenKind::NumericLiteral(46.0),
            TokenKind::NumericLiteral(7.89),
            TokenKind::NumericLiteral(42.0),
            TokenKind::NumericLiteral(5000.0),
            TokenKind::NumericLiteral(5000.0),
            TokenKind::NumericLiteral(0.005),
            TokenKind::NumericLiteral(2.0),
            TokenKind::NumericLiteral(83.0),
            TokenKind::NumericLiteral(999.0),
            TokenKind::NumericLiteral(10.0),
            TokenKind::NumericLiteral(0.1),
            TokenKind::NumericLiteral(10.0),
            TokenKind::NumericLiteral(10.0),
            TokenKind::NumericLiteral(0.0),
            TokenKind::NumericLiteral(0.12),
        ],
    );
}

#[test]
fn test_single_number_without_semicolon() {
    let mut lexer = Lexer::new("1", &mut Interner::new());
    lexer.lex().expect("failed to lex");
}

#[test]
fn test_number_followed_by_dot() {
    check_lexer(
        "1..",
        None,
        &[
            TokenKind::NumericLiteral(1.0),
            TokenKind::Punctuator(Punctuator::Dot),
        ],
    );
}

#[test]
fn test_regex_literal() {
    let mut int = Interner::new();
    check_lexer(
        "/(?:)/",
        None,
        &[TokenKind::regular_expression_literal(
            int.get_or_intern("(?:)"),
            int.get_or_intern(""),
        )],
    );
}

#[test]
fn test_regex_literal_flags() {
    let mut int = Interner::new();
    check_lexer(
        r"/\/[^\/]*\/*/gmi",
        None,
        &[TokenKind::regular_expression_literal(
            int.get_or_intern("\\/[^\\/]*\\/*"),
            int.get_or_intern("gmi"),
        )],
    );
}

#[test]
fn test_addition_no_spaces() {
    check_lexer(
        "1+1",
        None,
        &[
            TokenKind::NumericLiteral(1.0),
            TokenKind::Punctuator(Punctuator::Add),
            TokenKind::NumericLiteral(1.0),
        ],
    );
}

#[test]
fn test_addition_no_spaces_left_side() {
    check_lexer(
        "1+ 1",
        None,
        &[
            TokenKind::NumericLiteral(1.0),
            TokenKind::Punctuator(Punctuator::Add),
            TokenKind::NumericLiteral(1.0),
        ],
    );
}

#[test]
fn test_addition_no_spaces_right_side() {
    check_lexer(
        "1 +1",
        None,
        &[
            TokenKind::NumericLiteral(1.0),
            TokenKind::Punctuator(Punctuator::Add),
            TokenKind::NumericLiteral(1.0),
        ],
    );
}

#[test]
fn test_addition_no_spaces_e_number_left_side() {
    check_lexer(
        "1e2+ 1",
        None,
        &[
            TokenKind::NumericLiteral(100.0),
            TokenKind::Punctuator(Punctuator::Add),
            TokenKind::NumericLiteral(1.0),
        ],
    );
}

#[test]
fn test_addition_no_spaces_e_number_right_side() {
    check_lexer(
        "1 +1e3",
        None,
        &[
            TokenKind::NumericLiteral(100.0),
            TokenKind::Punctuator(Punctuator::Add),
            TokenKind::NumericLiteral(1_000.0),
        ],
    );
}

#[test]
fn test_addition_no_spaces_e_number() {
    check_lexer(
        "1e3+1e11",
        None,
        &[
            TokenKind::NumericLiteral(1000.0),
            TokenKind::Punctuator(Punctuator::Add),
            TokenKind::NumericLiteral(100_000_000_000.0),
        ],
    );
}
