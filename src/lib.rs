//! This is a Rust crate to do "command line parsing". No, I'm not talking
//! about parsing command line *arguments* that were passed to your program;
//! for that purpose, I recommend the excellent [Clap][1] crate (with features
//! `wrap_help` and `derive` enabled). What this crate does is take a *line of
//! text* and parse it like a command line. In other words, it parses shellish.
//!
//! This is useful if you're implementing any kind of interactive system where
//! a user needs to be able to input commands.
//!
//! [1]: https://crates.io/crates/clap
//!
//! # Usage
//!
//! Add `shellish_parse` to your `Cargo.toml`:
//!
//! ```toml
//! shellish_parse = "2.1"
//! ```
//!
//! Use `shellish_parse::parse` to parse some shellish:
//!
//! ```rust
//! let line = "Hello World";
//! assert_eq!(shellish_parse::parse(line, false).unwrap(), &[
//!     "Hello", "World"
//! ]);
//! ```
//!
//! The first parameter, a `&str`, is the line to parse. The second parameter,
//! a `bool`, is whether an unrecognized escape sequence should be an error:
//!
//! ```rust
//! let line = r#"In\mvalid"#; // note: raw string
//! assert_eq!(shellish_parse::parse(line, false).unwrap(), &[
//!     "In�valid"
//! ]);
//! assert_eq!(shellish_parse::parse(line, true).unwrap_err(),
//!     shellish_parse::ParseError::UnrecognizedEscape("\\m".to_string()));
//! ```
//!
//! You may want to use an alias to make it more convenient, if you're using it
//! in a lot of places:
//!
//! ```rust
//! use shellish_parse::parse as parse_shellish;
//! let line = "Hello World";
//! assert_eq!(parse_shellish(line, false).unwrap(), &[
//!     "Hello", "World"
//! ]);
//! ```
//!
//! Regular parse is great and everything, but sometimes you want to be able
//! to chain multiple commands on the same line. That's where `multiparse`
//! comes in:
//!
//! ```rust
//! let line = "Hello World; How are you?";
//! assert_eq!(shellish_parse::multiparse(line, true, &[";"]).unwrap(), &[
//!     (vec!["Hello".to_string(), "World".to_string()], Some(0)),
//!     (vec!["How".to_string(), "are".to_string(), "you?".to_string()], None),
//! ]);
//! ```
//!
//! (Since it returns a vec of tuples, it's rather awkward to phrase in tests.)
//!
//! You pass the separators you want to use. A single semicolon is probably
//! all you want. If you want to get really fancy, you can add arbitrarily many
//! different separators. Each command returned comes with the index of the
//! separator that terminated it:
//!
//! ```rust
//! let line = "test -f foo && pv foo | bar || echo no foo & echo wat";
//! assert_eq!(shellish_parse::multiparse(line, true, &["&&", "||", "&", "|",
//!                                                     ";"]).unwrap(), &[
//!     (vec!["test".to_string(), "-f".to_string(), "foo".to_string()], Some(0)),
//!     (vec!["pv".to_string(), "foo".to_string()], Some(3)),
//!     (vec!["bar".to_string()], Some(1)),
//!     (vec!["echo".to_string(), "no".to_string(), "foo".to_string()], Some(2)),
//!     (vec!["echo".to_string(), "wat".to_string()], None),
//! ]);
//! ```
//!
//! Since the separators are checked in the order passed, put longer
//! separators before shorter ones. If `"&"` preceded `"&&"` in the above call,
//! `"&"` would always be recognized first, and `"&&"` would never be
//! recognized.
//!
//! Extremely shellish things, like redirection or using parentheses to group
//! commands, are out of scope of this crate. If you want those things, you
//! might be writing an actual shell, and not just something shellish.
//!
//! # Syntax
//!
//! The syntax is heavily inspired by the UNIX Bourne shell. Quotation works
//! exactly like in said shell. Backslashes can also be used for escaping (and
//! more advanced usage, more like Rust strings than shellish). Unlike the real
//! Bourne shell, `parse_shellish` contains no form of variable substitution.
//!
//! ## Whitespace
//!
//! Elements are separated by one or more whitespace characters.
//!
//! ```rust
//! let line = "Hello there!";
//! assert_eq!(shellish_parse::parse(line, true).unwrap(), &[
//!     "Hello", "there!",
//! ])
//! ```
//!
//! Whitespace consists of spaces, tabs, or newlines. Whitespace before and
//! after the command line is ignored. Any combination and quantity of
//! whitespace between elements acts the same as a single space.
//!
//! ```rust
//! let line = "\tHello\n\t  there!    \n\n";
//! assert_eq!(shellish_parse::parse(line, true).unwrap(), &[
//!     "Hello", "there!",
//! ])
//! ```
//!
//! ## Backslash escapes
//!
//! (All example input strings in this section are given as raw strings. The
//! backslashes and quotation marks you see in them are literal.)
//!
//! You may escape any character with backslash.
//!
//! Backslash followed by an ASCII letter (26 letters `'A'` through `'Z'` and
//! `'a'` through `'z'`) or digit (`'0'` through `'9'`) has a special meaning.
//!
//! - `'n'`: Newline (U+000A LINE FEED)
//! - `'t'`: Tab (U+0009 CHARACTER TABULATION)
//! - Any other letter (and any digit) will either insert a � (U+FFFD
//!   REPLACEMENT CHARACTER) or cause a parse error, depending on the value you
//!   pass as the second parameter to `parse`.
//!
//! ```rust
//! let line = r#"General\t Kenobi\n"#;
//! assert_eq!(shellish_parse::parse(line, true).unwrap(), &[
//!     "General\t", "Kenobi\n",
//! ])
//! ```
//!
//! Backslash followed by a newline followed by any number of unescaped tabs or
//! spaces will give nothing, just like in Rust strings. (i.e. you may continue
//! a command line onto another line by preceding the linebreak with a
//! backslash)
//!
//! ```rust
//! let line = r#"You will die br\
//!               aver than most."#;
//! assert_eq!(shellish_parse::parse(line, true).unwrap(), &[
//!     "You", "will", "die", "braver", "than", "most."
//! ])
//! ```
//!
//! Backslash followed by anything else will give that character, ignoring any
//! special meaning it might otherwise have had.
//!
//! ```rust
//! let line = r#"Four\-score\ and\ seven \"years\" ago"#;
//! assert_eq!(shellish_parse::parse(line, true).unwrap(), &[
//!     "Four-score and seven", "\"years\"", "ago"
//! ])
//! ```
//!
//! Future versions may add more special characters. These will only be denoted
//! by letter(s) or digit(s). For all other characters, the handling of
//! backslash is guaranteed not to change.
//!
//! ## Quoting
//!
//! (All example input strings in this section are given as raw strings. The
//! backslashes and quotation marks you see in them are literal.)
//!
//! You may quote parts of the command line. The quoted text will all go into
//! the same element.
//!
//! ```rust
//! let line = r#"cp "Quotation Mark Test" "Quotation Mark Test Backup""#;
//! assert_eq!(shellish_parse::parse(line, true).unwrap(), &[
//!     "cp", "Quotation Mark Test", "Quotation Mark Test Backup"
//! ])
//! ```
//!
//! Quoting will *not* create a new element on its own.
//!
//! ```rust
//! let line = r#"I Probably Should Have"Added A Space!""#;
//! assert_eq!(shellish_parse::parse(line, true).unwrap(), &[
//!     "I", "Probably", "Should", "HaveAdded A Space!"
//! ])
//! ```
//!
//! There are two kinds of quotation. A double-quoted string will interpret
//! backslash escapes, including `\"`.
//!
//! ```rust
//! let line = r#"movie recommend "\"Swing it\" magistern""#;
//! assert_eq!(shellish_parse::parse(line, true).unwrap(), &[
//!     "movie", "recommend", "\"Swing it\" magistern"
//! ])
//! ```
//!
//! A single-quoted string **will not** interpret backslash escapes, not even
//! `\'`!
//!
//! ```rust
//! let line = r#"addendum 'and then he said "But I haven'\''t seen it, I \
//! just searched for '\''movies with quotes in their titles'\'' on IMDB and \
//! saw that it was popular"'"#;
//! assert_eq!(shellish_parse::parse(line, true).unwrap(), &[
//!     "addendum", "and then he said \"But I haven't seen it, I just \
//! searched for 'movies with quotes in their titles' on IMDB and saw that it \
//! was popular\""
//! ])
//! ```
//!
//! ## Continuation
//!
//! `parse` returns `Err(ParseResult::...)` on failure. There are three ways
//! parsing can fail:
//!
//! 1. Dangling backslash: `like this\`
//! 2. Unterminated string: `like "this`
//! 3. Unrecognized escape sequence: `like this\m`
//!
//! In the first two cases, parsing could succeed if there were only more input
//! to read. So you can handle these errors by prompting for more input, adding
//! it onto the end of the string, and trying again. The `needs_continuation`
//! method of `ParseResult` is here to help:
//!
//! ```rust
//! // note: raw strings
//! let input_lines = [r#"This is not a very \"#,
//!                    r#"long line, so why did \"#,
//!                    r#"we choose to 'force "#,
//!                    r#"continuation'?"#];
//! let mut input_iter = input_lines.into_iter();
//! let mut buf = input_iter.next().unwrap().to_string();
//! let result = loop {
//!     match shellish_parse::parse(&buf, true) {
//!         Err(x) if x.needs_continuation() => {
//!             buf.push('\n'); // don't forget this part!
//!             buf.push_str(input_iter.next().unwrap())
//!         },
//!         x => break x,
//!     }
//! };
//! assert_eq!(result.unwrap(), &[
//!     "This", "is", "not", "a", "very", "long", "line,", "so", "why", "did",
//!     "we", "choose", "to", "force \ncontinuation?"
//! ]);
//! ```
//!
//! # Legalese
//!
//! `shellish_parse` is copyright 2022, Solra Bizna, and licensed under either
//! of:
//!
//! - Apache License, Version 2.0
//!   ([LICENSE-APACHE](LICENSE-APACHE) or
//!   <http://www.apache.org/licenses/LICENSE-2.0>)
//! - MIT license
//!   ([LICENSE-MIT](LICENSE-MIT) or <http://opensource.org/licenses/MIT>)
//!
//! at your option.
//!
//! Unless you explicitly state otherwise, any contribution intentionally
//! submitted for inclusion in the `shellish_parse` crate by you, as defined
//! in the Apache-2.0 license, shall be dual licensed as above, without any
//! additional terms or conditions.

use std::{
    fmt::Display,
    error::Error,
};

/// A result of a failed command line parse.
///
/// Most of these errors can be resolved with additional user input. The
/// `needs_continuation` method is there to help. See
/// [the module-level documentation](index.html) for more information.
#[derive(Clone,Debug,PartialEq,Eq)]
pub enum ParseError {
    /// The command line ended with an unescaped backslash.
    DanglingBackslash,
    /// A string was still open when the command line ended.
    DanglingString,
    /// There was an unrecognized backslash escape sequence.
    UnrecognizedEscape(String),
}

impl ParseError {
    /// Returns `true` if this kind of ParseError will be resolved if the user
    /// provides more input.
    pub fn needs_continuation(&self) -> bool {
        match self {
            &ParseError::DanglingBackslash | &ParseError::DanglingString
                => true,
            _ => false,
        }
    }
}

impl Display for ParseError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            &ParseError::DanglingBackslash
                => write!(fmt, "dangling backslash"),
            &ParseError::DanglingString
                => write!(fmt, "dangling string"),
            &ParseError::UnrecognizedEscape(ref seq)
                => write!(fmt, "unrecognized escape sequence: {:?}", seq)
        }
    }
}

impl Error for ParseError {
}

/// Parse a shellish string into elements. This function will parse a single
/// command. See [the module-level documentation](index.html) for more
/// information.
///
/// - `input`: The string to parse.
/// - `strict_escapes`: If `false`, bad escape sequences will result in '�'.
///   If `true`, bad escape sequences will result in a `ParseError`. When in
///   doubt, pass `true`.
///
/// When parsing is successful, returns a vector containing each individual
/// element of the parsed command line.
///
/// A future version of this crate may replace `strict_escapes` with a more
/// featureful `Options` parameter. If this happens, the minor version will be
/// bumped *and* `From<bool>` will be leveraged to preserve compatibility with
/// the old meaning of this parameter.
pub fn parse(input: &str, strict_escapes: bool) -> Result<Vec<String>, ParseError> {
    match multiparse(input, strict_escapes, &[]) {
        Ok(mut x) => {
            assert!(x.len() <= 1);
            if x.len() == 0 {
                Ok(vec![])
            }
            else {
                let (command, sep) = x.swap_remove(0);
                assert_eq!(sep, None);
                Ok(command)
            }
        },
        Err(x) => Err(x),
    }
}

/// Parse a shellish string into elements. This function can parse multiple
/// commands on a single line, separated by any of the given list of
/// separators. See [the module-level documentation](index.html) for more
/// information.
///
/// - `input`: The string to parse.
/// - `strict_escapes`: If `false`, bad escape sequences will result in '�'.
///   If `true`, bad escape sequences will result in a `ParseError`. When in
///   doubt, pass `true`.
///
/// When parsing is successful, returns a vector containing tuples of
/// individual commands, along with the index of the separator that ended that
/// command. The last command may not have a separator, in which case it was
/// ended by the end of the string, rather than a separator.
///
/// A future version of this crate may replace `strict_escapes` with a more
/// featureful `Options` parameter. If this happens, the minor version will be
/// bumped *and* `From<bool>` will be leveraged to preserve compatibility with
/// the old meaning of this parameter.
pub fn multiparse(input: &str, strict_escapes: bool, separators: &[&str])
-> Result<Vec<(Vec<String>, Option<usize>)>, ParseError> {
    let inbytes = input.as_bytes();
    let mut ret = Vec::new();
    let mut cur_line = Vec::new();
    let mut pos = 0;
    let mut cur_arg: Option<Vec<u8>> = None;
    let mut cur_string = None;
    'outer: while pos < input.len() {
        if cur_string.is_none() {
            // Check separators iff we're not in a string
            let rem = &inbytes[pos..];
            for (index, separator) in separators.iter().enumerate() {
                if separator.len() > rem.len() { continue }
                if separator.as_bytes() == &rem[..separator.len()] {
                    // Hit a separator. Skip it and ship it.
                    pos += separator.len();
                    if let Some(cur_arg) = cur_arg.take() {
                        cur_line.push(unsafe {String::from_utf8_unchecked(cur_arg) });
                    }
                    ret.push((cur_line, Some(index)));
                    cur_line = Vec::new();
                    continue 'outer;
                }
            }
        }
        let nextb = inbytes[pos];
        if cur_string.is_none() && (nextb == b'\n' || nextb == b' ' || nextb == b'\t') {
            if let Some(cur_arg) = cur_arg.take() {
                cur_line.push(unsafe {String::from_utf8_unchecked(cur_arg) });
            }
            pos += 1;
        }
        else if Some(nextb) == cur_string {
            debug_assert!(cur_arg.is_some());
            cur_string = None;
            pos += 1;
        }
        else if nextb == b'\\' {
            cur_arg.get_or_insert_with(Vec::new);
            let cur_arg = cur_arg.as_mut().unwrap();
            pos += 1;
            if pos >= input.len() {
                return Err(ParseError::DanglingBackslash);
            }
            let escb = inbytes[pos];
            if escb.is_ascii_alphabetic() {
                match escb {
                    b't' => cur_arg.push(b'\t'),
                    b'n' => cur_arg.push(b'\n'),
                    _ => {
                        if strict_escapes {
                            return Err(ParseError::UnrecognizedEscape(input[pos-1..=pos].to_string()))
                        }
                        else {
                            cur_arg.extend_from_slice("\u{FFFD}".as_bytes());
                        }
                    },
                }
            }
            else if escb == b'\n' {
                // eat any subsequent non-newline whitespace we find
                while pos + 1 < inbytes.len()
                      && (inbytes[pos+1] == b' ' || inbytes[pos+1] == b'\t') {
                    pos += 1;
                }
            }
            else {
                cur_arg.push(escb);
            }
            pos += 1;
        }
        else if cur_string.is_none() && nextb == b'"' {
            cur_arg.get_or_insert_with(Vec::new);
            cur_string = Some(b'"');
            pos += 1;
        }
        else if cur_string.is_none() && nextb == b'\'' {
            cur_arg.get_or_insert_with(Vec::new);
            cur_string = Some(b'\'');
            pos += 1;
        }
        else {
            cur_arg.get_or_insert_with(Vec::new).push(nextb);
            pos += 1;
        }
    }
    if cur_string.is_some() {
        return Err(ParseError::DanglingString);
    }
    if let Some(cur_arg) = cur_arg.take() {
        cur_line.push(unsafe {String::from_utf8_unchecked(cur_arg) });
    }
    if !cur_line.is_empty() {
        ret.push((cur_line, None));
    }
    Ok(ret)
}
