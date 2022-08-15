# shellish_parse

This is a Rust crate to do "command line parsing". No, I'm not talking
about parsing command line *arguments* that were passed to your program;
for that purpose, I recommend the excellent [Clap][1] crate (with features
`wrap_help` and `derive` enabled). What this crate does is take a *line of
text* and parse it like a command line. In other words, it parses shellish.

This is useful if you're implementing any kind of interactive system where
a user needs to be able to input commands.

[1]: https://crates.io/crates/clap

## Usage

Add `shellish_parse` to your `Cargo.toml`:

```toml
shellish_parse = "1.0"
```

Use `shellish_parse::parse` to parse some shellish:

```rust
let line = "Hello World";
assert_eq!(shellish_parse::parse(line, false).unwrap(), &[
    "Hello", "World"
]);
```

The first parameter, a `&str`, is the line to parse. The second parameter,
a `bool`, is whether an unrecognized escape sequence should be an error:

```rust
let line = r#"In\mvalid"#; // note: raw string
assert_eq!(shellish_parse::parse(line, false).unwrap(), &[
    "In�valid"
]);
assert_eq!(shellish_parse::parse(line, true).unwrap_err(),
    shellish_parse::ParseError::UnrecognizedEscape("\\m"));
```

You may want to use an alias to make it more convenient, if you're using it
in a lot of places:

```rust
use shellish_parse::parse as parse_shellish;
let line = "Hello World";
assert_eq!(parse_shellish(line, false).unwrap(), &[
    "Hello", "World"
]);
```

## Syntax

The syntax is heavily inspired by the UNIX Bourne shell. Quotation works
exactly like in said shell. Backslashes can also be used for escaping (and
more advanced usage, more like Rust strings than shellish). Unlike the real
Bourne shell, `parse_shellish` contains no form of variable substitution.

### Whitespace

Elements are separated by one or more whitespace characters.

```rust
let line = "Hello there!";
assert_eq!(shellish_parse::parse(line, true).unwrap(), &[
    "Hello", "there!",
])
```

Whitespace consists of spaces, tabs, or newlines. Whitespace before and
after the command line is ignored. Any combination and quantity of
whitespace between elements acts the same as a single space.

```rust
let line = "\tHello\n\t  there!    \n\n";
assert_eq!(shellish_parse::parse(line, true).unwrap(), &[
    "Hello", "there!",
])
```

### Backslash escapes

(All example input strings in this section are given as raw strings. The
backslashes and quotation marks you see in them are literal.)

You may escape any character with backslash.

Backslash followed by an ASCII letter (26 letters `'A'` through `'Z'` and
`'a'` through `'z'`) or digit (`'0'` through `'9'`) has a special meaning.

- `'n'`: Newline (U+000A LINE FEED)
- `'t'`: Tab (U+0009 CHARACTER TABULATION)
- Any other letter (and any digit) will either insert a � (U+FFFD
  REPLACEMENT CHARACTER) or cause a parse error, depending on the value you
  pass as the second parameter to `parse`.

```rust
let line = r#"General\t Kenobi\n"#;
assert_eq!(shellish_parse::parse(line, true).unwrap(), &[
    "General\t", "Kenobi\n",
])
```

Backslash followed by a newline followed by any number of unescaped tabs or
spaces will give nothing, just like in Rust strings. (i.e. you may continue
a command line onto another line by preceding the linebreak with a
backslash)

```rust
let line = r#"You will die br\
              aver than most."#;
assert_eq!(shellish_parse::parse(line, true).unwrap(), &[
    "You", "will", "die", "braver", "than", "most."
])
```

Backslash followed by anything else will give that character, ignoring any
special meaning it might otherwise have had.

```rust
let line = r#"Four\-score\ and\ seven \"years\" ago"#;
assert_eq!(shellish_parse::parse(line, true).unwrap(), &[
    "Four-score and seven", "\"years\"", "ago"
])
```

Future versions may add more special characters. These will only be denoted
by letter(s) or digit(s). For all other characters, the handling of
backslash is guaranteed not to change.

### Quoting

(All example input strings in this section are given as raw strings. The
backslashes and quotation marks you see in them are literal.)

You may quote parts of the command line. The quoted text will all go into
the same element.

```rust
let line = r#"cp "Quotation Mark Test" "Quotation Mark Test Backup""#;
assert_eq!(shellish_parse::parse(line, true).unwrap(), &[
    "cp", "Quotation Mark Test", "Quotation Mark Test Backup"
])
```

Quoting will *not* create a new element on its own.

```rust
let line = r#"I Probably Should Have"Added A Space!""#;
assert_eq!(shellish_parse::parse(line, true).unwrap(), &[
    "I", "Probably", "Should", "HaveAdded A Space!"
])
```

There are two kinds of quotation. A double-quoted string will interpret
backslash escapes, including `\"`.

```rust
let line = r#"movie recommend "\"Swing it\" magistern""#;
assert_eq!(shellish_parse::parse(line, true).unwrap(), &[
    "movie", "recommend", "\"Swing it\" magistern"
])
```

A single-quoted string **will not** interpret backslash escapes, not even
`\'`!

```rust
let line = r#"addendum 'and then he said "But I haven'\''t seen it, I \
just searched for '\''movies with quotes in their titles'\'' on IMDB and \
saw that it was popular"'"#;
assert_eq!(shellish_parse::parse(line, true).unwrap(), &[
    "addendum", "and then he said \"But I haven't seen it, I just \
searched for 'movies with quotes in their titles' on IMDB and saw that it \
was popular\""
])
```

### Continuation

`parse` returns `Err(ParseResult::...)` on failure. There are three ways
parsing can fail:

1. Dangling backslash: `like this\`
2. Unterminated string: `like "this`
3. Unrecognized escape sequence: `like this\m`

In the first two cases, parsing could succeed if there were only more input
to read. So you can handle these errors by prompting for more input, adding
it onto the end of the string, and trying again. The `needs_continuation`
method of `ParseResult` is here to help:

```rust
// note: raw strings
let input_lines = [r#"This is not a very \"#,
                   r#"long line, so why did \"#,
                   r#"we choose to 'force "#,
                   r#"continuation'?"#];
let mut input_iter = input_lines.into_iter();
let mut buf = input_iter.next().unwrap().to_string();
let result = loop {
    match shellish_parse::parse(&buf, true) {
        Err(x) if x.needs_continuation() => {
            buf.push('\n'); // don't forget this part!
            buf.push_str(input_iter.next().unwrap())
        },
        x => break x,
    }
};
assert_eq!(result.unwrap(), &[
    "This", "is", "not", "a", "very", "long", "line,", "so", "why", "did",
    "we", "choose", "to", "force \ncontinuation?"
]);
```

## Legalese

`shellish_parse` is copyright 2022, Solra Bizna, and licensed under either
of:

- Apache License, Version 2.0
  ([LICENSE-APACHE](LICENSE-APACHE) or
  <http://www.apache.org/licenses/LICENSE-2.0>)
- MIT license
  ([LICENSE-MIT](LICENSE-MIT) or <http://opensource.org/licenses/MIT>)

at your option.

Unless you explicitly state otherwise, any contribution intentionally
submitted for inclusion in the `shellish_parse` crate by you, as defined
in the Apache-2.0 license, shall be dual licensed as above, without any
additional terms or conditions.

License: MIT OR Apache-2.0
