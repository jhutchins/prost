use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::{
        complete::{alpha1, char, digit0, digit1, hex_digit1, none_of, oct_digit1, one_of, space0},
        is_alphanumeric, is_hex_digit, is_oct_digit,
    },
    combinator::{opt, recognize},
    error::{Error, ErrorKind},
    multi::{many1, separated_list1},
    sequence::{delimited, tuple},
    AsChar, IResult, InputIter, Parser, Slice,
};

/// Constant is a type that represents a protobuf constant and each type contains value of the appropriate rust type
pub enum Constant {
    Ident(String),
    Int(i32),
    Float(f64),
    String(String),
    Bool(bool),
}

/// Parse a protobuf [constant][0]
///
/// ```ebpf
/// constant = fullIdent | ( [ "-" | "+" ] intLit ) | ( [ "-" | "+" ] floatLit ) |
///                 strLit | boolLit
/// ```
///
/// [0]: https://developers.google.com/protocol-buffers/docs/reference/proto3-spec#constant
pub fn constant(input: &[u8]) -> IResult<&[u8], Constant> {
    alt((
        full_ident.map(Constant::Ident),
        tuple((opt(alt((char('+'), char('-')))), int_literal)).map(|(sign, mut val)| {
            if let Some('-') = sign {
                val *= -1;
            }
            Constant::Int(val)
        }),
        tuple((opt(alt((char('+'), char('-')))), float_literal)).map(|(sign, mut val)| {
            if let Some('-') = sign {
                val *= -1f64;
            }
            Constant::Float(val)
        }),
        string_literal.map(Constant::String),
        alt((tag(b"true"), tag(b"false"))).map(|val| Constant::Bool(val == b"true")),
    ))(input)
}

/// Parse a protobuf [emptyStatement][0]
///
/// ```ebpf
/// emptyStatement = ";"
/// ```
///
/// [0]: https://developers.google.com/protocol-buffers/docs/reference/proto3-spec#emptystatement
pub fn empty_statement(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = space0(input)?;
    let (input, _) = char(';')(input)?;
    Ok((input, ()))
}

/// Parse a protobuf [fullIdent][0]
///
/// ```ebnf
/// fullIdent = ident { "." ident }
/// ident = letter { letter | decimalDigit | "_" }
/// ```
///
/// [0]: https://developers.google.com/protocol-buffers/docs/reference/proto3-spec#identifiers
pub fn full_ident(input: &[u8]) -> IResult<&[u8], String> {
    let (input, idents) = separated_list1(char('.'), ident)(input)?;
    Ok((input, idents.join(".")))
}

/// Parse a protobuf [ident][0]
///
/// ```ebnf
/// ident = letter { letter | decimalDigit | "_" }
/// ```
///
/// [0]: https://developers.google.com/protocol-buffers/docs/reference/proto3-spec#identifiers
pub fn ident(input: &[u8]) -> IResult<&[u8], String> {
    let (input, head) = alpha1(input)?;
    let head = unsafe { String::from_utf8_unchecked(head.to_vec()) };
    let (input, tail) = take_while(|c| is_alphanumeric(c) || c == b'_')(input)?;
    let tail = unsafe { String::from_utf8_unchecked(tail.to_vec()) };
    Ok((input, format!("{head}{tail}")))
}

/// Parse a protobuf [intLit][0]
///
/// ```ebpf
/// intLit     = decimalLit | octalLit | hexLit
/// decimalLit = ( "1" … "9" ) { decimalDigit }
/// octalLit   = "0" { octalDigit }
/// hexLit     = "0" ( "x" | "X" ) hexDigit { hexDigit }
/// ```
///
/// [0]: https://developers.google.com/protocol-buffers/docs/reference/proto3-spec#integer_literals
pub fn int_literal(input: &[u8]) -> IResult<&[u8], i32> {
    if let (input, Some(_)) = opt(char('0'))(input)? {
        if let (input, Some(_)) = opt(alt((char('x'), char('X'))))(input)? {
            let (input, num) = hex_digit1(input)?;
            let num = unsafe { std::str::from_utf8_unchecked(num) };
            Ok((input, i32::from_str_radix(num, 16).unwrap()))
        } else {
            let (input, num) = oct_digit1(input)?;
            let num = unsafe { std::str::from_utf8_unchecked(num) };
            Ok((input, i32::from_str_radix(num, 8).unwrap()))
        }
    } else {
        let (input, num) = digit1(input)?;
        let num = unsafe { std::str::from_utf8_unchecked(num) };
        Ok((input, num.parse().unwrap()))
    }
}

/// Parse a protobuf [messageType][0]
///
/// ```ebnf
/// messageType = [ "." ] { ident "." } messageName
/// ```
///
/// [0]: https://developers.google.com/protocol-buffers/docs/reference/proto3-spec#identifiers
pub fn message_type(input: &[u8]) -> IResult<&[u8], String> {
    let (input, value) =
        recognize(tuple((opt(char('.')), separated_list1(char('.'), ident))))(input)?;
    let mt = unsafe { String::from_utf8_unchecked(value.to_vec()) };
    Ok((input, mt))
}

/// Parse a protobuf [strLit][0]
///
/// ```ebpf
/// strLit = ( "'" { charValue } "'" ) | ( '"' { charValue } '"' )
/// charValue = hexEscape | octEscape | charEscape | /[^\0\n\\]/
/// hexEscape = '\' ( "x" | "X" ) hexDigit hexDigit
/// octEscape = '\' octalDigit octalDigit octalDigit
/// charEscape = '\' ( "a" | "b" | "f" | "n" | "r" | "t" | "v" | '\' | "'" | '"' )
/// ```
///
/// [0]: https://developers.google.com/protocol-buffers/docs/reference/proto3-spec#string_literals
pub fn string_literal(input: &[u8]) -> IResult<&[u8], String> {
    alt((
        delimited(char('"'), char_vals, char('"')),
        delimited(char('\''), char_vals, char('\'')),
    ))(input)
}

/// Helper method to generate parsers to get charaters that match a particular condition
fn char_matches<F>(cond: F) -> impl Fn(&[u8]) -> IResult<&[u8], char>
where
    F: Fn(u8) -> bool,
{
    move |i: &[u8]| match (i).iter_elements().next().map(|t| (t.as_char(), cond(t))) {
        Some((c, true)) => Ok((i.slice(c.len()..), c)),
        _ => Err(nom::Err::Error(Error::new(i, ErrorKind::Char))),
    }
}

/// Parser for a protobuf [charValue][0]
///
/// ```ebpf
/// charValue = hexEscape | octEscape | charEscape | /[^\0\n\\]/
/// ```
///
/// [0]: https://developers.google.com/protocol-buffers/docs/reference/proto3-spec#string_literals
fn char_vals(input: &[u8]) -> IResult<&[u8], String> {
    let (input, result) = many1(alt((
        escaped_hex,
        escaped_oct,
        escaped_char,
        none_of(r#"[^\0\n\\]'""#).map(|c| format!("{c}")),
    )))(input)?;
    Ok((input, result.join("")))
}

/// Parser for a charEscaped [charValue][0]
///
/// ```ebpf
/// charEscape = '\' ( "a" | "b" | "f" | "n" | "r" | "t" | "v" | '\' | "'" | '"' )
/// ```
///
/// [0]: https://developers.google.com/protocol-buffers/docs/reference/proto3-spec#string_literals
fn escaped_char(input: &[u8]) -> IResult<&[u8], String> {
    let (output, _) = char('\\')(input)?;
    let (output, _) = one_of(r#"abfnrtv\'""#)(output)?;
    let bytes = input[0..2].into();
    // safe because we checked all the values are valid UTF
    Ok((output, unsafe { String::from_utf8_unchecked(bytes) }))
}

/// Parser for a protobuf [hexEscape][0]
///
/// ```ebpf
/// hexEscape = '\' ( "x" | "X" ) hexDigit hexDigit
/// ```
///
/// [0]: https://developers.google.com/protocol-buffers/docs/reference/proto3-spec#string_literals
fn escaped_hex(input: &[u8]) -> IResult<&[u8], String> {
    let (output, _) = char('\\')(input)?;
    let (output, _) = alt((char('x'), char('X')))(output)?;
    let (output, _) = char_matches(is_hex_digit)(output)?;
    let (output, _) = char_matches(is_hex_digit)(output)?;
    let bytes = input[0..4].into();
    // safe because we checked all the values are valid UTF
    Ok((output, unsafe { String::from_utf8_unchecked(bytes) }))
}

/// Parser for a protobuf [octEscape][0]
///
/// ```ebpf
/// octEscape = '\' octalDigit octalDigit octalDigit
/// ```
///
/// [0]: https://developers.google.com/protocol-buffers/docs/reference/proto3-spec#string_literals
fn escaped_oct(input: &[u8]) -> IResult<&[u8], String> {
    let (output, _) = char('\\')(input)?;
    let (output, _) = char_matches(is_oct_digit)(output)?;
    let (output, _) = char_matches(is_oct_digit)(output)?;
    let (output, _) = char_matches(is_oct_digit)(output)?;
    let bytes = input[0..4].into();
    // safe because we checked all the values are valid UTF
    Ok((output, unsafe { String::from_utf8_unchecked(bytes) }))
}

/// Parser for a protobuf [exponent][0]
///
/// ```ebpf
/// exponent  = ( "e" | "E" ) [ "+" | "-" ] decimals
/// ```
///
/// [0]: https://developers.google.com/protocol-buffers/docs/reference/proto3-spec#floating-point-literals
fn exponent(input: &[u8]) -> IResult<&[u8], &[u8]> {
    recognize(tuple((
        alt((char('e'), char('E'))),
        opt(alt((char('+'), char('-')))),
        digit1,
    )))(input)
}

/// Parser for a protobuf [floatLit][0]
///
/// ```ebpf
/// floatLit = ( decimals "." [ decimals ] [ exponent ] | decimals exponent | "."decimals [ exponent ] ) | "inf" | "nan"
/// decimals  = decimalDigit { decimalDigit }
/// exponent  = ( "e" | "E" ) [ "+" | "-" ] decimals
/// ```
///
/// [0]: https://developers.google.com/protocol-buffers/docs/reference/proto3-spec#floating-point-literals
fn float_literal(input: &[u8]) -> IResult<&[u8], f64> {
    let (input, data) = alt((
        recognize(tuple((digit1, char('.'), digit0, opt(exponent)))),
        recognize(tuple((digit1, exponent))),
        recognize(tuple((char('.'), digit1, opt(exponent)))),
        tag("inf"),
        tag("nan"),
    ))(input)?;
    let num = unsafe { std::str::from_utf8_unchecked(data) };
    Ok((input, num.parse().unwrap()))
}
