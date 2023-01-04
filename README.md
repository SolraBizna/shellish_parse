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
shellish_parse = "2.2"
```

Use `shellish_parse::parse` to parse some shellish:

```rust
let line = "Hello World";
assert_eq!(shellish_parse::parse(line, false).unwrap(), &[
    "Hello", "World"
]);
```

The first parameter, a `&str`, is the line to parse. The second parameter,
a can be a `bool`, indicating whether an unrecognized escape sequence
should be an error:

```rust
let line = r#"In\mvalid"#; // note: raw string
assert_eq!(shellish_parse::parse(line, false).unwrap(), &[
    "In�valid"
]);
assert_eq!(shellish_parse::parse(line, true).unwrap_err(),
    shellish_parse::ParseError::UnrecognizedEscape("\\m".to_string()));
```

Or a [`ParseOptions`](struct.ParseOptions.html), giving you more control
(see that struct's documentation for more details):

```rust
let line = r#"In\mvalid"#; // note: raw string
let options = ParseOptions::new().no_strict_escapes();
assert_eq!(shellish_parse::parse(line, options).unwrap(), &[
    "In�valid"
]);
let options = ParseOptions::new();
assert_eq!(shellish_parse::parse(line, options).unwrap_err(),
    shellish_parse::ParseError::UnrecognizedEscape("\\m".to_string()));
```

You may want to use an alias to make calling this function more convenient
if you're using it in a lot of places:

```rust
use shellish_parse::parse as parse_shellish;
let line = "Hello World";
assert_eq!(parse_shellish(line, false).unwrap(), &[
    "Hello", "World"
]);
```

And putting your preferred `ParseOptions` into a `const` can save you some
typing:

```rust
const SHELLISH_OPTIONS: ParseOptions = ParseOptions::new()
        .allow_comments_within_elements();
use shellish_parse::parse as parse_shellish;
let line = "This line contains a com#ment";
assert_eq!(parse_shellish(line, SHELLISH_OPTIONS).unwrap(), &[
    "This", "line", "contains", "a", "com"
]);
```

Regular parse is great and everything, but sometimes you want to be able
to chain multiple commands on the same line. That's where `multiparse`
comes in:

```rust
let line = "Hello World; How are you?";
assert_eq!(shellish_parse::multiparse(line, SHELLISH_OPTIONS, &[";"])
           .unwrap(), &[
    (vec!["Hello".to_string(), "World".to_string()], Some(0)),
    (vec!["How".to_string(), "are".to_string(), "you?".to_string()], None),
]);
```

(Since it returns a vec of tuples, it's rather awkward to phrase in tests.)

You pass the separators you want to use. A single semicolon is probably
all you want. If you want to get really fancy, you can add arbitrarily many
different separators. Each command returned comes with the index of the
separator that terminated it:

```rust
let line = "test -f foo && pv foo | bar || echo no foo & echo wat";
assert_eq!(shellish_parse::multiparse(line, SHELLISH_OPTIONS,
                                      &["&&", "||", "&", "|", ";"])
           .unwrap(), &[
    (vec!["test".to_string(), "-f".to_string(), "foo".to_string()], Some(0)),
    (vec!["pv".to_string(), "foo".to_string()], Some(3)),
    (vec!["bar".to_string()], Some(1)),
    (vec!["echo".to_string(), "no".to_string(), "foo".to_string()], Some(2)),
    (vec!["echo".to_string(), "wat".to_string()], None),
]);
```

Since the separators are checked in the order passed, put longer
separators before shorter ones. If `"&"` preceded `"&&"` in the above call,
`"&"` would always be recognized first, and `"&&"` would never be
recognized.

Extremely shellish things, like redirection or using parentheses to group
commands, are out of scope of this crate. If you want those things, you
might be writing an actual shell, and not just something shellish.

## Syntax

The syntax is heavily inspired by the UNIX Bourne shell. Quotation works
exactly like in said shell. Backslashes can also be used for escaping (and
more advanced usage, more like Rust strings than shellish). Unlike the real
Bourne shell, `parse_shellish` contains no form of variable substitution.

### Whitespace

Elements are separated by one or more whitespace characters.

```rust
let line = "Hello there!";
assert_eq!(shellish_parse::parse(line, SHELLISH_OPTIONS).unwrap(), &[
    "Hello", "there!",
])
```

Whitespace consists of spaces, tabs, or newlines. Whitespace before and
after the command line is ignored. Any combination and quantity of
whitespace between elements acts the same as a single space.

```rust
let line = "\tHello\n\t  there!    \n\n";
assert_eq!(shellish_parse::parse(line, SHELLISH_OPTIONS).unwrap(), &[
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
assert_eq!(shellish_parse::parse(line, SHELLISH_OPTIONS).unwrap(), &[
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
assert_eq!(shellish_parse::parse(line, SHELLISH_OPTIONS).unwrap(), &[
    "You", "will", "die", "braver", "than", "most."
])
```

Backslash followed by anything else will give that character, ignoring any
special meaning it might otherwise have had.

```rust
let line = r#"Four\-score\ and\ seven \"years\" ago"#;
assert_eq!(shellish_parse::parse(line, SHELLISH_OPTIONS).unwrap(), &[
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
assert_eq!(shellish_parse::parse(line, SHELLISH_OPTIONS).unwrap(), &[
    "cp", "Quotation Mark Test", "Quotation Mark Test Backup"
])
```

Quoting will *not* create a new element on its own.

```rust
let line = r#"I Probably Should Have"Added A Space!""#;
assert_eq!(shellish_parse::parse(line, SHELLISH_OPTIONS).unwrap(), &[
    "I", "Probably", "Should", "HaveAdded A Space!"
])
```

There are two kinds of quotation. A double-quoted string will interpret
backslash escapes, including `\"`.

```rust
let line = r#"movie recommend "\"Swing it\" magistern""#;
assert_eq!(shellish_parse::parse(line, SHELLISH_OPTIONS).unwrap(), &[
    "movie", "recommend", "\"Swing it\" magistern"
])
```

A single-quoted string **will not** interpret backslash escapes, not even
`\'`!

```rust
let line = r#"addendum 'and then he said "But I haven'\''t seen it, I \
just searched for '\''movies with quotes in their titles'\'' on IMDB and \
saw that it was popular"'"#;
assert_eq!(shellish_parse::parse(line, SHELLISH_OPTIONS).unwrap(), &[
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
    match shellish_parse::parse(&buf, SHELLISH_OPTIONS) {
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

### Comments

By default, comments are delimited by a `#` character.

```rust
let line = "Comment test. #comments #sayinghashtagoutloud";
assert_eq!(shellish_parse::parse(line, SHELLISH_OPTIONS).unwrap(), &[
    "Comment", "test."
])
```

You can change this to any other character using
[`ParseOptions`](struct.ParseOptions.html):

```rust
const SHELLISH_OPTIONS: ParseOptions = ParseOptions::new()
        .comment_char(Some('%'));
let line = "bind lmbutton Interact % make left mouse button interact";
assert_eq!(shellish_parse::parse(line, SHELLISH_OPTIONS).unwrap(), &[
    "bind", "lmbutton", "Interact"
])
```

You can also disable comment parsing entirely:

```rust
const SHELLISH_OPTIONS: ParseOptions = ParseOptions::new()
        .comment_char(None);
let line = "Comment test. #comments #sayinghashtagoutloud";
assert_eq!(shellish_parse::parse(line, SHELLISH_OPTIONS).unwrap(), &[
    "Comment", "test.", "#comments", "#sayinghashtagoutloud"
])
```

By default, comments are not allowed in the middle of an element. This
behavior matches the Bourne shell. You can make it so that any comment
character, found outside a string, will be accepted as the beginning of a
comment:

```rust
let line = "Comment that breaks an el#ement.";
const SHELLISH_OPTIONS: ParseOptions = ParseOptions::new();
assert_eq!(shellish_parse::parse(line, SHELLISH_OPTIONS).unwrap(), &[
    "Comment", "that", "breaks", "an", "el#ement."
]);
const SHELLISH_OPTIONS_2: ParseOptions = ParseOptions::new()
        .allow_comments_within_elements();
assert_eq!(shellish_parse::parse(line, SHELLISH_OPTIONS_2).unwrap(), &[
    "Comment", "that", "breaks", "an", "el"
]);
```

## Legalese

`shellish_parse` is copyright 2022-2023, Solra Bizna, and licensed under
either of:

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
