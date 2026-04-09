//! This is a parser for JSON5.
//! JSON5 extends JSON with comments, trailing commas, unquoted keys, single quotes, and more.
//! Run it with the following command:
//! cargo run --example json5 -- examples/sample.json5

use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::prelude::*;
use std::{collections::HashMap, env, fs, process};

#[derive(Clone, Debug)]
pub enum Json5 {
    Invalid,
    Null,
    Bool(bool),
    Str(String),
    Num(f64),
    Array(Vec<Json5>),
    Object(HashMap<String, Json5>),
}

fn parser<'a>() -> impl Parser<'a, &'a str, Json5, extra::Err<Rich<'a, char>>> {
    recursive(|value| {
        // Single-line comment: // ...
        let single_comment = just("//")
            .then(any().and_is(just('\n').not()).repeated())
            .ignored()
            .boxed();

        // Multi-line comment: /* ... */
        let multi_comment = just("/*")
            .then(any().and_is(just("*/").not()).repeated())
            .then(just("*/"))
            .ignored()
            .boxed();

        // Whitespace with comments
        let ws = choice((
            text::whitespace().at_least(1).ignored(),
            single_comment,
            multi_comment,
        ))
        .repeated()
        .boxed();

        let digits = text::digits(10).to_slice();

        let exp = just('e')
            .or(just('E'))
            .then(one_of("+-").or_not())
            .then(digits);

        // Hexadecimal numbers: 0x...
        let hex_number = just("0x")
            .or(just("0X"))
            .ignore_then(text::digits(16).to_slice())
            .map(|s: &str| i64::from_str_radix(s, 16).unwrap() as f64)
            .boxed();

        // Decimal numbers (including leading/trailing decimal points)
        // Leading decimal point: .5 or .5e10
        let leading_decimal = one_of("+-")
            .or_not()
            .then(just('.'))
            .then(digits.clone())
            .then(exp.clone().or_not())
            .to_slice()
            .map(|s: &str| {
                // Prepend '0' after sign to make it parseable
                if s.starts_with('+') || s.starts_with('-') {
                    let sign = &s[..1];
                    format!("{}0{}", sign, &s[1..]).parse().unwrap()
                } else {
                    format!("0{}", s).parse().unwrap()
                }
            })
            .boxed();

        // Regular decimal with both integer and fractional parts: 5.5 or 5.5e10
        let regular_decimal = one_of("+-")
            .or_not()
            .then(text::int(10))
            .then(just('.'))
            .then(digits.clone())
            .then(exp.clone().or_not())
            .to_slice()
            .map(|s: &str| s.parse().unwrap())
            .boxed();

        // Trailing decimal point: 5. or 5.e10
        let trailing_decimal = one_of("+-")
            .or_not()
            .then(text::int(10))
            .then(just('.'))
            .then(exp.clone().or_not())
            .to_slice()
            .map(|s: &str| {
                // Append '0' after the decimal point
                if s.contains('e') || s.contains('E') {
                    let parts: Vec<&str> = s.split(|c| c == 'e' || c == 'E').collect();
                    let e_pos = s.find(|c| c == 'e' || c == 'E').unwrap();
                    format!("{}0{}", parts[0], &s[e_pos..]).parse().unwrap()
                } else {
                    format!("{}0", s).parse().unwrap()
                }
            })
            .boxed();

        // Integer with optional exponent: 5 or 5e10
        let integer_number = one_of("+-")
            .or_not()
            .then(text::int(10))
            .then(exp.or_not())
            .to_slice()
            .map(|s: &str| s.parse().unwrap())
            .boxed();

        // Special numeric values
        let special_number = choice((
            just("Infinity").to(f64::INFINITY),
            just("+Infinity").to(f64::INFINITY),
            just("-Infinity").to(f64::NEG_INFINITY),
            just("NaN").to(f64::NAN),
        ))
        .boxed();

        // Order matters: try more specific patterns first
        let number = choice((
            hex_number,
            special_number,
            regular_decimal,   // 5.5 or 5.5e10 - must come before trailing_decimal
            leading_decimal,   // .5 or .5e10
            trailing_decimal,  // 5. or 5.e10
            integer_number,    // 5 or 5e10 - must come last
        ))
        .boxed();

        // Escape sequences
        let escape = just('\\')
            .then(choice((
                just('\\'),
                just('/'),
                just('"'),
                just('\''),
                just('b').to('\x08'),
                just('f').to('\x0C'),
                just('n').to('\n'),
                just('r').to('\r'),
                just('t').to('\t'),
                just('v').to('\x0B'),
                just('0').to('\0'),
                just('u').ignore_then(text::digits(16).exactly(4).to_slice().validate(
                    |digits, e, emitter| {
                        char::from_u32(u32::from_str_radix(digits, 16).unwrap()).unwrap_or_else(
                            || {
                                emitter.emit(Rich::custom(e.span(), "invalid unicode character"));
                                '\u{FFFD}' // unicode replacement character
                            },
                        )
                    },
                )),
            )))
            .ignored()
            .boxed();

        // Double-quoted strings
        let double_string = none_of("\\\"")
            .ignored()
            .or(escape.clone())
            .repeated()
            .to_slice()
            .map(ToString::to_string)
            .delimited_by(just('"'), just('"'))
            .boxed();

        // Single-quoted strings
        let single_string = none_of("\\'")
            .ignored()
            .or(escape)
            .repeated()
            .to_slice()
            .map(ToString::to_string)
            .delimited_by(just('\''), just('\''))
            .boxed();

        let string = double_string.or(single_string).boxed();

        let array = value
            .clone()
            .separated_by(just(',').padded_by(ws.clone()).recover_with(skip_then_retry_until(
                any().ignored(),
                one_of(",]").ignored(),
            )))
            .allow_trailing()
            .collect()
            .padded_by(ws.clone())
            .delimited_by(
                just('['),
                just(']')
                    .ignored()
                    .recover_with(via_parser(end()))
                    .recover_with(skip_then_retry_until(any().ignored(), end())),
            )
            .boxed();

        // Unquoted keys: identifiers or reserved words
        let unquoted_key = text::ascii::ident()
            .map(ToString::to_string)
            .boxed();

        let key = string.clone().or(unquoted_key).boxed();

        let member = key.then_ignore(just(':').padded_by(ws.clone())).then(value);
        let object = member
            .clone()
            .separated_by(just(',').padded_by(ws.clone()).recover_with(skip_then_retry_until(
                any().ignored(),
                one_of(",}").ignored(),
            )))
            .allow_trailing()
            .collect()
            .padded_by(ws.clone())
            .delimited_by(
                just('{'),
                just('}')
                    .ignored()
                    .recover_with(via_parser(end()))
                    .recover_with(skip_then_retry_until(any().ignored(), end())),
            )
            .boxed();

        choice((
            just("null").to(Json5::Null),
            just("true").to(Json5::Bool(true)),
            just("false").to(Json5::Bool(false)),
            number.map(Json5::Num),
            string.map(Json5::Str),
            array.map(Json5::Array),
            object.map(Json5::Object),
        ))
        .recover_with(via_parser(nested_delimiters(
            '{',
            '}',
            [('[', ']')],
            |_| Json5::Invalid,
        )))
        .recover_with(via_parser(nested_delimiters(
            '[',
            ']',
            [('{', '}')],
            |_| Json5::Invalid,
        )))
        .recover_with(skip_then_retry_until(
            any().ignored(),
            one_of(",]}").ignored(),
        ))
        .padded_by(ws)
    })
}

fn print_help() {
    println!("json5 - A JSON5 parser with detailed error reporting");
    println!();
    println!("USAGE:");
    println!("    json5 <FILE>...");
    println!("    json5 --help");
    println!();
    println!("ARGS:");
    println!("    <FILE>...    One or more JSON5 files to parse");
    println!();
    println!("OPTIONS:");
    println!("    --help, -h    Print this help message");
    println!();
    println!("EXAMPLES:");
    println!("    json5 config.json5");
    println!("    json5 examples/sample.json5 examples/valid.json5");
    println!("    json5 *.json5");
    println!();
    println!("FEATURES:");
    println!("    - Comments: // single-line and /* multi-line */");
    println!("    - Trailing commas in arrays and objects");
    println!("    - Unquoted object keys");
    println!("    - Single-quoted strings");
    println!("    - Hexadecimal numbers (0xFF)");
    println!("    - Special values: Infinity, -Infinity, NaN");
    println!("    - Leading/trailing decimal points (.5 or 5.)");
}

fn parse_args() -> Option<Vec<String>> {
    let args: Vec<String> = env::args().collect();

    // Check for --help
    if args.iter().any(|arg| arg == "--help" || arg == "-h") {
        print_help();
        return None;
    }

    let mut file_paths = Vec::new();

    for arg in args.iter().skip(1) {
        match arg.as_str() {
            arg if arg.starts_with("--") => {
                eprintln!("Error: Unknown option '{}'", arg);
                eprintln!("Use --help for usage information");
                process::exit(1);
            }
            _ => {
                file_paths.push(arg.to_string());
            }
        }
    }

    if file_paths.is_empty() {
        eprintln!("Error: No file arguments provided");
        eprintln!("Use --help for usage information");
        process::exit(1);
    }

    Some(file_paths)
}

fn parse_file(file_path: String) -> bool {
    let src = match fs::read_to_string(&file_path) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file '{}': {}", file_path, err);
            return false;
        }
    };

    println!("Parsing file: {}", file_path);

    let (_json5, errs) = parser().parse(src.trim()).into_output_errors();

    let error_count = errs.len();

    if error_count > 0 {
        errs.into_iter().for_each(|e| {
            Report::build(ReportKind::Error, (file_path.clone(), e.span().into_range()))
                .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
                .with_message(e.to_string())
                .with_label(
                    Label::new((file_path.clone(), e.span().into_range()))
                        .with_message(e.reason().to_string())
                        .with_color(Color::Red),
                )
                .finish()
                .print(sources([(file_path.clone(), src.clone())]))
                .unwrap()
        });
        println!("{}", "=".repeat(80));
    }

    error_count == 0
}

fn main() {
    let file_paths = match parse_args() {
        Some(paths) => paths,
        None => return, // --help was shown
    };

    let mut failure_count = 0;

    for file_path in file_paths.into_iter() {
        if !parse_file(file_path) {
            failure_count += 1;
        }
    }

    // Exit with non-zero code if any errors were found
    if failure_count > 0 {
        process::exit(1);
    }
}
