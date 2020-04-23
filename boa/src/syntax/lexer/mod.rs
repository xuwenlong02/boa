//! A lexical analyzer for JavaScript source code.
//!
//! The Lexer splits its input source code into a sequence of input elements called tokens, represented by the [Token](../ast/token/struct.Token.html) structure.
//! It also removes whitespace and comments and attaches them to the next token.

#[cfg(test)]
mod tests;

use crate::syntax::ast::{
    punc::Punctuator,
    token::{Token, TokenKind},
};
use std::{
    char::{decode_utf16, from_u32},
    error, fmt,
    iter::Peekable,
    str::{self, Bytes, FromStr},
    u8,
};

macro_rules! vop {
    ($this:ident, $assign_op:expr, $op:expr) => ({
        let preview = $this.preview_next().ok_or_else(|| LexerError::new("Could not preview next value"))?;
        match preview {
            b'=' => {
                $this.next();
                $this.column_number += 1;
                $assign_op
            }
            _ => $op,
        }
    });
    ($this:ident, $assign_op:expr, $op:expr, {$($case:pat => $block:expr), +}) => ({
        let preview = $this.preview_next().ok_or_else(|| LexerError::new("Could not preview next value"))?;
        match preview {
            b'=' => {
                $this.next();
                $this.column_number += 1;
                $assign_op
            },
            $($case => {
                $this.next();
                $this.column_number += 1;
                $block
            })+,
            _ => $op
        }
    });
    ($this:ident, $op:expr, {$($case:pat => $block:expr),+}) => {
        let preview = $this.preview_next().ok_or_else(|| LexerError::new("Could not preview next value"))?;
        match preview {
            $($case => {
                $this.next()?;
                $this.column_number += 1;
                $block
            })+,
            _ => $op
        }
    }
}

macro_rules! op {
    ($this:ident, $assign_op:expr, $op:expr) => ({
        let punc = vop!($this, $assign_op, $op);
        $this.push_punc(punc);
    });
    ($this:ident, $assign_op:expr, $op:expr, {$($case:pat => $block:expr),+}) => ({
        let punc = vop!($this, $assign_op, $op, {$($case => $block),+});
        $this.push_punc(punc);
    });
    ($this:ident, $op:expr, {$($case:pat => $block:expr),+}) => ({
        let punc = vop!($this, $op, {$($case => $block),+});
        $this.push_punc();
    });
}

/// An error that occurred during lexing or compiling of the source input.
#[derive(Debug, Clone)]
pub struct LexerError {
    details: String,
}

impl LexerError {
    fn new<M>(msg: M) -> Self
    where
        M: Into<String>,
    {
        Self {
            details: msg.into(),
        }
    }
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.details)
    }
}

impl error::Error for LexerError {
    fn description(&self) -> &str {
        &self.details
    }

    fn cause(&self) -> Option<&dyn error::Error> {
        // Generic error, underlying cause isn't tracked.
        None
    }
}

/// A lexical analyzer for JavaScript source code
#[derive(Debug)]
pub struct Lexer<'a> {
    /// The list of tokens generated so far.
    pub tokens: Vec<Token>,
    /// The current line number in the script.
    line_number: u64,
    /// The current column number in the script.
    column_number: u64,
    /// The full source code as a byte iterator.
    buffer: Peekable<Bytes<'a>>,
}

impl<'a> Lexer<'a> {
    /// Returns a Lexer with a buffer inside
    ///
    /// # Arguments
    ///
    /// * `buffer` - A string slice that holds the source code.
    /// The buffer needs to have a lifetime as long as the Lexer instance itself
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// let buffer = std::fs::read_to_string("yourSourceCode.js").unwrap();
    /// let lexer = boa::syntax::lexer::Lexer::new(&buffer);
    /// ```
    pub fn new(buffer: &'a str) -> Lexer<'a> {
        Lexer {
            tokens: Vec::new(),
            line_number: 1,
            column_number: 0,
            buffer: buffer.bytes().peekable(),
        }
    }

    /// Push tokens onto the token queue
    fn push_token(&mut self, tk: TokenKind) {
        self.tokens
            .push(Token::new(tk, self.line_number, self.column_number))
    }

    /// Push a punctuation token
    fn push_punc(&mut self, punc: Punctuator) {
        self.push_token(TokenKind::Punctuator(punc));
    }

    /// next fetches the next token and return it, or a LexerError if there are no more.
    fn next(&mut self) -> u8 {
        self.buffer.next().expect(
            "No more more bytes to consume from input stream, \
             use preview_next() first to check before calling next()",
        )
    }

    /// Preview the next character but don't actually increment
    fn preview_next(&mut self) -> Option<u8> {
        self.buffer.peek().copied()
    }

    /// Preview a char x indexes further in buf, without incrementing
    fn preview_multiple_next(&mut self, nb_next: usize) -> Option<u8> {
        let mut next_peek = None;

        for (i, x) in self.buffer.clone().enumerate() {
            if i >= nb_next {
                break;
            }

            next_peek = Some(x);
        }

        next_peek
    }

    /// Utility Function, while ``f(u8)`` is true, read chars and move curser.
    /// All chars are returned as a string
    fn take_byte_while<F>(&mut self, mut f: F) -> Result<String, LexerError>
    where
        F: FnMut(&u8) -> bool,
    {
        let mut bytes = Vec::new();
        while self.buffer.peek().is_some()
            && f(&self.preview_next().expect("Could not preview next value"))
        {
            bytes.push(self.next());
        }

        String::from_utf8(bytes).map_err(|e| LexerError::new(e.to_string()))
    }

    /// Compares the character passed in to the next character, if they match true is returned and the buffer is incremented
    fn next_is(&mut self, peek: u8) -> bool {
        let result = self.preview_next() == Some(peek);
        if result {
            self.buffer.next();
        }
        result
    }

    /// Checks if a character is whitespace as per the ECMAScript specification.
    ///
    /// More information: <https://tc39.es/ecma262/#sec-white-space>
    fn is_whitespace(&mut self, ch: u8) -> Result<bool, LexerError> {
        // Note that the Rust `char::is_whitespace` function and the ECMAScript standard use different sets
        // of characters as whitespaces:
        //   - Rust uses `\p{White_Space}`,
        //   - ecma standard uses `\{Space_Separator}` + `\u{0009}`, `\u{000B}`, `\u{000C}`, `\u{FEFF}`
        //
        // Explicit whitespace: see https://tc39.es/ecma262/#table-32
        println!("is whitespace ch: {:X}", ch);
        match ch {
            0x09 | 0x0B | 0x0C | 0x20 | 0xA0 => Ok(true),
            _ => match self.utf8_char(ch)? {
                None => Ok(false),
                Some('\u{FEFF}')
                | Some('\u{2000}'..='\u{200A}')
                | Some('\u{202F}')
                | Some('\u{205F}')
                | Some('\u{3000}') => Ok(true),
                _ => Ok(false),
            },
        }
    }

    /// Checks if a character is a line terminator as per the ECMAScript specification.
    ///
    /// More information: <https://tc39.es/ecma262/#sec-line-terminators>
    fn is_line_terminator(&self, ch: u8) -> Result<bool, LexerError> {
        match ch {
            0x0A | 0x0D => Ok(true),
            _ => match self.utf8_char(ch)? {
                None => Ok(false),
                Some('\u{2028}') | Some('\u{2029}') => Ok(true),
                _ => Ok(false),
            },
        }
    }

    /// Retrieves the character as an UTF-8 character, if it's not ASCII.
    fn utf8_char(&self, first_byte: u8) -> Result<Option<char>, LexerError> {
        dbg!(&self.tokens);
        println!("first byte: {:b} ({0:X})", first_byte);
        if first_byte < 0x80 {
            // 0b0xxx_xxxx
            println!("1 byte");
            Ok(None)
        } else {
            let num_bytes = if first_byte < 0xE0 {
                // 0b110x_xxxx
                2
            } else if first_byte < 0xF0 {
                // 0b1110_xxxx
                3
            } else {
                // 0b1111_0xxx
                4
            };
            dbg!(num_bytes);
            println!("iter: {:X?}", &self.buffer);

            let mut bytes = Vec::with_capacity(num_bytes);
            bytes.push(first_byte);

            for _ in 1..num_bytes {
                bytes.push(self.buffer.next().ok_or_else(|| {
                    LexerError::new("unexpected EOF while parsing an UTF-8 character")
                })?)
            }

            println!("bytes: {:X?}", &bytes);
            println!("next: {:X?}", self.buffer.peek());

            let char_str = String::from_utf8(bytes)
                .map_err(|e| LexerError::new(e.to_string()))
                .expect("error"); //?;

            dbg!(&char_str);
            dbg!(char_str.chars().next().expect("character disappeared"));
            Ok(Some(
                char_str.chars().next().expect("character disappeared"),
            ))
        }
    }

    /// Checks if the given byte is an ASCII digit with a given radix.
    fn is_ascii_digit(byte: u8, radix: u32) -> bool {
        char::from(byte).is_digit(radix)
    }

    /// Reads an integer with the given radix.
    fn read_integer_radix(&mut self, radix: u32) -> Result<u64, LexerError> {
        self.next();
        let mut buf = Vec::new();
        while let Some(ch) = self.preview_next() {
            if Self::is_ascii_digit(ch, radix) {
                buf.push(self.next());
            } else {
                break;
            }
        }

        u64::from_str_radix(
            str::from_utf8(&buf).map_err(|e| LexerError::new(e.to_string()))?,
            radix,
        )
        .map_err(|_| LexerError::new("Could not convert value to u64"))
    }

    fn check_after_numeric_literal(&mut self) -> Result<(), LexerError> {
        match self.preview_next() {
            Some(ch)
                if ch.is_ascii_alphabetic() || ch == b'$' || ch == b'_' || ch.is_ascii_digit() =>
            {
                Err(LexerError::new("NumericLiteral token must not be followed by IdentifierStart nor DecimalDigit characters"))
            }
            Some(_) => Ok(()),
            None => Ok(())
        }
    }

    pub fn lex(&mut self) -> Result<(), LexerError> {
        loop {
            // Check if we've reached the end
            if self.preview_next().is_none() {
                return Ok(());
            }
            self.column_number += 1;
            let ch = self.next();
            match ch {
                b'"' | b'\'' => {
                    let mut buf = String::new();
                    loop {
                        if self.preview_next().is_none() {
                            return Err(LexerError::new("Unterminated String"));
                        }
                        match self.next() {
                            b'\'' if ch == b'\'' => {
                                break;
                            }
                            b'"' if ch == b'"' => {
                                break;
                            }
                            b'\\' => {
                                if self.preview_next().is_none() {
                                    return Err(LexerError::new("Unterminated String"));
                                }
                                let escape = self.next();
                                if escape != b'\n' {
                                    let escaped_ch = match escape {
                                        b'n' => '\n',
                                        b'r' => '\r',
                                        b't' => '\t',
                                        b'b' => '\x08',
                                        b'f' => '\x0c',
                                        b'0' => '\0',
                                        b'x' => {
                                            let mut nums = String::with_capacity(2);
                                            for _ in 0_u8..2 {
                                                if self.preview_next().is_none() {
                                                    return Err(LexerError::new(
                                                        "Unterminated String",
                                                    ));
                                                }
                                                nums.push(char::from(self.next()));
                                            }
                                            self.column_number += 2;
                                            let as_num = match u64::from_str_radix(&nums, 16) {
                                                Ok(v) => v,
                                                Err(_) => 0,
                                            };
                                            match from_u32(as_num as u32) {
                                                Some(v) => v,
                                                None => panic!(
                                                    "{}:{}: {} is not a valid unicode scalar value",
                                                    self.line_number, self.column_number, as_num
                                                ),
                                            }
                                        }
                                        b'u' => {
                                            // There are 2 types of codepoints. Surragate codepoints and unicode codepoints.
                                            // UTF-16 could be surrogate codepoints, "\uXXXX\uXXXX" which make up a single unicode codepoint.
                                            // We will need to loop to make sure we catch all UTF-16 codepoints
                                            // Example Test: https://github.com/tc39/test262/blob/ee3715ee56744ccc8aeb22a921f442e98090b3c1/implementation-contributed/v8/mjsunit/es6/unicode-escapes.js#L39-L44

                                            // Support \u{X..X} (Unicode Codepoint)
                                            if self.next_is(b'{') {
                                                let s = self
                                                    .take_byte_while(u8::is_ascii_alphanumeric)
                                                    .expect("Could not read chars");

                                                // We know this is a single unicode codepoint, convert to u32
                                                let as_num = match u32::from_str_radix(&s, 16) {
                                                    Ok(v) => v,
                                                    Err(_) => 0,
                                                };
                                                let c = from_u32(as_num).ok_or_else(|| {
                                                    LexerError::new(
                                                        "Invalid Unicode escape sequence",
                                                    )
                                                })?;

                                                if self.preview_next().is_none() {
                                                    return Err(LexerError::new(
                                                        "Unterminated String",
                                                    ));
                                                }
                                                self.next(); // '}'
                                                self.column_number +=
                                                    (s.len() as u64).wrapping_add(3);
                                                c
                                            } else {
                                                let mut codepoints: Vec<u16> = vec![];
                                                loop {
                                                    // Collect each character after \u e.g \uD83D will give "D83D"
                                                    let s = self
                                                        .take_byte_while(u8::is_ascii_alphanumeric)
                                                        .expect("Could not read chars");

                                                    // Convert to u16
                                                    let as_num = match u16::from_str_radix(&s, 16) {
                                                        Ok(v) => v,
                                                        Err(_) => 0,
                                                    };

                                                    codepoints.push(as_num);
                                                    self.column_number +=
                                                        (s.len() as u64).wrapping_add(2);

                                                    // Check for another UTF-16 codepoint
                                                    if self.next_is(b'\\') && self.next_is(b'u') {
                                                        continue;
                                                    }
                                                    break;
                                                }

                                                // codepoints length should either be 1 (unicode codepoint) or 2 (surrogate codepoint).
                                                // Rust's decode_utf16 will deal with it regardless
                                                decode_utf16(codepoints.iter().cloned())
                                                    .next()
                                                    .expect("Could not get next codepoint")
                                                    .expect("Could not get next codepoint")
                                            }
                                        }
                                        b'\'' | b'"' | b'\\' => char::from(escape),
                                        ch => {
                                            let details = format!(
                                                "{}:{}: Invalid escape `{}`",
                                                self.line_number, self.column_number, ch
                                            );
                                            return Err(LexerError { details });
                                        }
                                    };
                                    buf.push(escaped_ch);
                                }
                            }
                            next_ch => {
                                if let Some(ch) = self.utf8_char(next_ch)? {
                                    buf.push(ch);
                                } else {
                                    buf.push(char::from(next_ch));
                                }
                            }
                        }
                    }
                    let str_length = buf.chars().count() as u64;
                    self.push_token(TokenKind::StringLiteral(buf));
                    // Why +1? Quotation marks are not included,
                    // So technically it would be +2, (for both " ") but we want to be 1 less
                    // to compensate for the incrementing at the top
                    self.column_number += str_length.wrapping_add(1);
                }
                b'0' => {
                    let num = match self.preview_next() {
                        None => {
                            self.push_token(TokenKind::NumericLiteral(0_f64));
                            return Ok(());
                        }
                        Some(b'x') | Some(b'X') => self.read_integer_radix(16)? as f64,
                        Some(b'o') | Some(b'O') => self.read_integer_radix(8)? as f64,
                        Some(b'b') | Some(b'B') => self.read_integer_radix(2)? as f64,
                        Some(ch) if (ch.is_ascii_digit() || ch == b'.') => {
                            let mut buf = Vec::new();
                            // LEGACY OCTAL (ONLY FOR NON-STRICT MODE)
                            let mut gone_decimal = ch == b'.';
                            while let Some(next_ch) = self.preview_next() {
                                match next_ch {
                                    c if Self::is_ascii_digit(next_ch, 8) => {
                                        buf.push(c);
                                        self.next();
                                    }
                                    b'8' | b'9' | b'.' => {
                                        gone_decimal = true;
                                        buf.push(next_ch);
                                        self.next();
                                    }
                                    _ => {
                                        break;
                                    }
                                }
                            }
                            if gone_decimal {
                                f64::from_str(
                                    str::from_utf8(&buf)
                                        .map_err(|e| LexerError::new(e.to_string()))?,
                                )
                                .map_err(|_e| LexerError::new("Could not convert value to f64"))?
                            } else if buf.is_empty() {
                                0.0
                            } else {
                                (u64::from_str_radix(
                                    str::from_utf8(&buf)
                                        .map_err(|e| LexerError::new(e.to_string()))?,
                                    8,
                                )
                                .map_err(|_e| LexerError::new("Could not convert value to u64"))?)
                                    as f64
                            }
                        }
                        Some(_) => 0.0,
                    };

                    self.push_token(TokenKind::NumericLiteral(num));

                    //11.8.3
                    if let Err(e) = self.check_after_numeric_literal() {
                        return Err(e);
                    };
                }
                _ if Self::is_ascii_digit(ch, 10) => {
                    let mut buf = vec![ch];
                    'digitloop: while let Some(ch) = self.preview_next() {
                        match ch {
                            b'.' => loop {
                                buf.push(self.next());

                                let c = match self.preview_next() {
                                    Some(ch) => ch,
                                    None => break,
                                };

                                match c {
                                    b'e' | b'E' => {
                                        match self
                                            .preview_multiple_next(2)
                                            .map(char::from)
                                            .unwrap_or_default()
                                            .to_digit(10)
                                        {
                                            Some(0..=9) | None => {
                                                buf.push(self.next());
                                            }
                                            _ => {
                                                break 'digitloop;
                                            }
                                        }
                                    }
                                    _ => {
                                        if !Self::is_ascii_digit(c, 10) {
                                            break 'digitloop;
                                        }
                                    }
                                }
                            },
                            b'e' | b'E' => {
                                match self
                                    .preview_multiple_next(2)
                                    .map(char::from)
                                    .unwrap_or_default()
                                    .to_digit(10)
                                {
                                    Some(0..=9) | None => {
                                        buf.push(self.next());
                                    }
                                    _ => {
                                        break;
                                    }
                                }
                                buf.push(self.next());
                            }
                            b'+' | b'-' => {
                                break;
                            }
                            _ if Self::is_ascii_digit(ch, 10) => {
                                buf.push(self.next());
                            }
                            _ => break,
                        }
                    }
                    // TODO make this a bit more safe -------------------------------VVVV
                    self.push_token(TokenKind::NumericLiteral(
                        f64::from_str(
                            str::from_utf8(&buf).map_err(|e| LexerError::new(e.to_string()))?,
                        )
                        .map_err(|_| LexerError::new("Could not convert value to f64"))?,
                    ))
                }
                _ if ch.is_ascii_alphabetic() || ch == b'$' || ch == b'_' => {
                    let mut buf = vec![ch];
                    while let Some(ch) = self.preview_next() {
                        if ch >= 0x80 {
                            unimplemented!("UTF-8 identifiers");
                        } else if ch.is_ascii_alphabetic()
                            || Self::is_ascii_digit(ch, 10)
                            || ch == b'_'
                        {
                            buf.push(self.next());
                        } else {
                            break;
                        }
                    }

                    let len = buf.len();

                    self.push_token(match &buf[..] {
                        b"true" => TokenKind::BooleanLiteral(true),
                        b"false" => TokenKind::BooleanLiteral(false),
                        b"null" => TokenKind::NullLiteral,
                        _ => {
                            let str_buf = String::from_utf8(buf)
                                .map_err(|e| LexerError::new(e.to_string()))?;
                            if let Ok(keyword) = str_buf.parse() {
                                // TODO: optimize Keyword to be able to be created from a u8 slice.
                                TokenKind::Keyword(keyword)
                            } else {
                                TokenKind::Identifier(str_buf)
                            }
                        }
                    });
                    // Move position forward the length of keyword
                    self.column_number += (len.wrapping_sub(1)) as u64;
                }
                b';' => self.push_punc(Punctuator::Semicolon),
                b':' => self.push_punc(Punctuator::Colon),
                b'.' => {
                    // . or ...
                    if self.next_is(b'.') {
                        if self.next_is(b'.') {
                            self.push_punc(Punctuator::Spread);
                            self.column_number += 2;
                        } else {
                            return Err(LexerError::new("Expecting Token ."));
                        }
                    } else {
                        self.push_punc(Punctuator::Dot);
                    };
                }
                b'(' => self.push_punc(Punctuator::OpenParen),
                b')' => self.push_punc(Punctuator::CloseParen),
                b',' => self.push_punc(Punctuator::Comma),
                b'{' => self.push_punc(Punctuator::OpenBlock),
                b'}' => self.push_punc(Punctuator::CloseBlock),
                b'[' => self.push_punc(Punctuator::OpenBracket),
                b']' => self.push_punc(Punctuator::CloseBracket),
                b'?' => self.push_punc(Punctuator::Question),
                // Comments
                b'/' => {
                    if let Some(ch) = self.preview_next() {
                        match ch {
                            // line comment
                            b'/' => {
                                while self.preview_next().is_some() {
                                    if self.next() == b'\n' {
                                        break;
                                    }
                                }
                                self.line_number += 1;
                                self.column_number = 0;
                            }
                            // block comment
                            b'*' => {
                                let mut lines = 0;
                                loop {
                                    if self.preview_next().is_none() {
                                        return Err(LexerError::new(
                                            "Unterminated Multiline Comment",
                                        ));
                                    }
                                    match self.next() {
                                        b'*' => {
                                            if self.next_is(b'/') {
                                                break;
                                            }
                                        }
                                        next_ch => {
                                            if next_ch == b'\n' {
                                                lines += 1;
                                            }
                                        }
                                    }
                                }
                                self.line_number += lines;
                                self.column_number = 0;
                            }
                            // division, assigndiv or regex literal
                            _ => {
                                // if we fail to parse a regex literal, store a copy of the current
                                // buffer to restore later on
                                let original_buffer = self.buffer.clone();
                                // first, try to parse a regex literal
                                let mut body = Vec::new();
                                let mut regex = false;
                                loop {
                                    self.column_number += 1;
                                    match self.buffer.next() {
                                        // end of body
                                        Some(b'/') => {
                                            regex = true;
                                            break;
                                        }
                                        // newline/eof not allowed in regex literal
                                        Some(n) if self.is_line_terminator(n)? => {
                                            self.column_number = 0;
                                            if n != b'\r' {
                                                self.line_number += 1;
                                            }
                                            break;
                                        }
                                        None => {
                                            self.column_number -= 1;
                                            break;
                                        }
                                        // escape sequence
                                        Some(b'\\') => {
                                            body.push(b'\\');
                                            if self.preview_next().is_none() {
                                                break;
                                            }
                                            match self.next() {
                                                // newline not allowed in regex literal
                                                ch if self.is_line_terminator(ch)? => break,
                                                ch => body.push(ch),
                                            }
                                        }
                                        Some(ch) => body.push(ch),
                                    }
                                }
                                if regex {
                                    // body was parsed, now look for flags
                                    let flags = self.take_byte_while(u8::is_ascii_alphabetic)?;
                                    self.push_token(TokenKind::RegularExpressionLiteral(
                                        String::from_utf8(body)
                                            .map_err(|e| LexerError::new(e.to_string()))?,
                                        flags,
                                    ));
                                } else {
                                    // failed to parse regex, restore original buffer position and
                                    // parse either div or assigndiv
                                    self.buffer = original_buffer;
                                    if self.next_is(b'=') {
                                        self.push_token(TokenKind::Punctuator(
                                            Punctuator::AssignDiv,
                                        ));
                                    } else {
                                        self.push_token(TokenKind::Punctuator(Punctuator::Div));
                                    }
                                }
                            }
                        }
                    } else {
                        return Err(LexerError::new("Expecting Token /,*,= or regex"));
                    }
                }
                b'*' => op!(self, Punctuator::AssignMul, Punctuator::Mul, {
                    b'*' => vop!(self, Punctuator::AssignPow, Punctuator::Exp)
                }),
                b'+' => op!(self, Punctuator::AssignAdd, Punctuator::Add, {
                    b'+' => Punctuator::Inc
                }),
                b'-' => op!(self, Punctuator::AssignSub, Punctuator::Sub, {
                    b'-' => {
                        Punctuator::Dec
                    }
                }),
                b'%' => op!(self, Punctuator::AssignMod, Punctuator::Mod),
                b'|' => op!(self, Punctuator::AssignOr, Punctuator::Or, {
                    b'|' => Punctuator::BoolOr
                }),
                b'&' => op!(self, Punctuator::AssignAnd, Punctuator::And, {
                    b'&' => Punctuator::BoolAnd
                }),
                b'^' => op!(self, Punctuator::AssignXor, Punctuator::Xor),
                b'=' => op!(self, if self.next_is(b'=') {
                    Punctuator::StrictEq
                } else {
                    Punctuator::Eq
                }, Punctuator::Assign, {
                    b'>' => {
                        Punctuator::Arrow
                    }
                }),
                b'<' => op!(self, Punctuator::LessThanOrEq, Punctuator::LessThan, {
                    b'<' => vop!(self, Punctuator::AssignLeftSh, Punctuator::LeftSh)
                }),
                b'>' => op!(self, Punctuator::GreaterThanOrEq, Punctuator::GreaterThan, {
                    b'>' => vop!(self, Punctuator::AssignRightSh, Punctuator::RightSh, {
                        b'>' => vop!(self, Punctuator::AssignURightSh, Punctuator::URightSh)
                    })
                }),
                b'!' => op!(
                    self,
                    vop!(self, Punctuator::StrictNotEq, Punctuator::NotEq),
                    Punctuator::Not
                ),
                b'~' => self.push_punc(Punctuator::Neg),
                b'\r' => {
                    self.column_number = 0;
                }
                ch if self.is_line_terminator(ch)? => {
                    self.push_token(TokenKind::LineTerminator);
                    self.line_number += 1;
                    self.column_number = 0;
                }
                _ if self.is_whitespace(ch)? => (),
                _ => {
                    let details = format!(
                        "{}:{}: Unexpected '{}'",
                        self.line_number,
                        self.column_number,
                        char::from(ch)
                    );
                    return Err(LexerError { details });
                }
            }
        }
    }
}
