use convert_case::{Case, Casing};
use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while},
    character::{
        complete::{
            alpha1, char, digit0, digit1, hex_digit1, line_ending, multispace1, none_of,
            not_line_ending, oct_digit1, one_of, space0, space1,
        },
        is_alphabetic, is_hex_digit, is_oct_digit,
    },
    combinator::{opt, recognize},
    error::{Error, ErrorKind},
    multi::{many0, many1, separated_list1},
    sequence::{delimited, preceded, terminated, tuple},
    AsChar, IResult, InputIter, Parser, Slice,
};
use prost_types::{
    descriptor_proto::ReservedRange, enum_descriptor_proto::EnumReservedRange, DescriptorProto,
    EnumDescriptorProto, EnumValueDescriptorProto, FieldDescriptorProto, FileDescriptorProto,
    MessageOptions, MethodDescriptorProto, OneofDescriptorProto, ServiceDescriptorProto,
};
use std::io::{self, Result};

pub fn parse(input: Vec<u8>) -> Result<FileDescriptorProto> {
    match parse_proto(&input) {
        Ok((b"", result)) => Ok(result),
        Ok((remaining, _)) => Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "cannot parse all of file, failed at: {}",
                String::from_utf8_lossy(remaining)
            ),
        )),
        Err(err) => Err(io::Error::new(io::ErrorKind::Other, err.to_string())),
    }
}

fn parse_proto(input: &[u8]) -> IResult<&[u8], FileDescriptorProto> {
    let (input, _) = many0(line_ending)(input)?;
    let (input, syntax) = syntax(input)?;

    let mut result = FileDescriptorProto::default();
    let mut input = input;
    loop {
        let (next, _) = many0(line_ending)(input)?;
        if let (next, Some(import)) = opt(import)(next)? {
            match import {
                Import::Public(import) => {
                    result
                        .public_dependency
                        .push(result.dependency.len().try_into().unwrap());
                    result.dependency.push(import);
                }
                Import::Standard(import) => {
                    result.dependency.push(import);
                }
                Import::Weak(import) => {
                    result
                        .weak_dependency
                        .push(result.dependency.len().try_into().unwrap());
                    result.dependency.push(import);
                }
            }
            input = next;
            continue;
        }
        if let (next, Some(package)) = opt(package)(next)? {
            result.package = Some(package);
            input = next;
            continue;
        }
        if let (next, Some(())) = opt(option)(next)? {
            input = next;
            continue;
        }
        if let (next, Some(enum_type)) = opt(enumeration)(next)? {
            result.enum_type.push(enum_type);
            input = next;
            continue;
        }
        if let (next, Some(message_type)) = opt(message(&syntax == "proto2"))(next)? {
            result.message_type.push(message_type);
            input = next;
            continue;
        }
        if let (next, Some(_)) = opt(comment)(next)? {
            input = next;
            continue;
        }
        if let (next, Some(service)) = opt(service)(next)? {
            result.service.push(service);
            input = next;
            continue;
        }
        input = next;
        break;
    }
    result.syntax = Some(syntax);
    Ok((input, result))
}

fn service(input: &[u8]) -> IResult<&[u8], ServiceDescriptorProto> {
    let (input, _) = space0(input)?;
    let (input, _) = tag("service")(input)?;
    let (input, _) = space1(input)?;
    let (input, name) = ident(input)?;
    let (input, _) = space0(input)?;
    let (input, method) = delimited(
        char('{'),
        many0(alt((
            option.map(|_| None),
            empty_statement.map(|_| None),
            multispace1.map(|_| None),
            comment.map(|_| None),
            rpc.map(Some),
        ))),
        char('}'),
    )(input)?;
    let method = method.iter().filter_map(Option::to_owned).collect();
    Ok((
        input,
        ServiceDescriptorProto {
            name: Some(name),
            method,
            ..ServiceDescriptorProto::default()
        },
    ))
}

fn rpc(input: &[u8]) -> IResult<&[u8], MethodDescriptorProto> {
    let (input, _) = space0(input)?;
    let (input, _) = tag("rpc")(input)?;
    let (input, _) = space1(input)?;
    let (input, name) = ident.map(Some).parse(input)?;
    let (input, _) = space0(input)?;
    let (input, (client_streaming, input_type)) = delimited(
        terminated(char('('), space0),
        tuple((
            opt(terminated(tag("stream").map(|_| true), space1)),
            message_type.map(Some),
        )),
        preceded(space0, char(')')),
    )(input)?;
    let (input, _) = delimited(space0, tag("returns"), space0)(input)?;
    let (input, (server_streaming, output_type)) = delimited(
        terminated(char('('), space0),
        tuple((
            opt(terminated(tag("stream").map(|_| true), space1)),
            message_type.map(Some),
        )),
        preceded(space0, char(')')),
    )(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = opt(delimited(
        char('['),
        separated_list1(char(','), option_parts),
        char(']'),
    ))(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char(';')(input)?;
    Ok((
        input,
        MethodDescriptorProto {
            name,
            input_type,
            output_type,
            options: None,
            client_streaming,
            server_streaming,
        },
    ))
}

fn comment(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = alt((
        preceded(tag("//"), not_line_ending),
        delimited(tag("/*"), take_until("*/"), tag("*/")),
    ))(input)?;
    Ok((input, ()))
}

fn package(input: &[u8]) -> IResult<&[u8], String> {
    let (input, _) = space0(input)?;
    let (input, _) = tag("package")(input)?;
    let (input, _) = space1(input)?;
    let (input, name) = full_ident(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char(';')(input)?;
    Ok((input, name))
}

fn full_ident(input: &[u8]) -> IResult<&[u8], String> {
    let (input, idents) = separated_list1(char('.'), ident)(input)?;
    Ok((input, idents.join(".")))
}

fn ident(input: &[u8]) -> IResult<&[u8], String> {
    let (input, head) = alpha1(input)?;
    let head = unsafe { String::from_utf8_unchecked(head.to_vec()) };
    let (input, tail) = take_while(is_alphanumericplus)(input)?;
    let tail = unsafe { String::from_utf8_unchecked(tail.to_vec()) };
    Ok((input, format!("{head}{tail}")))
}

fn is_alphanumericplus(c: u8) -> bool {
    is_alphabetic(c) || c == b'_'
}

#[derive(Debug, Eq, PartialEq)]
enum Import {
    Public(String),
    Standard(String),
    Weak(String),
}

fn import(input: &[u8]) -> IResult<&[u8], Import> {
    let (input, _) = space0(input)?;
    let (input, _) = tag("import")(input)?;
    let (input, _) = space1(input)?;
    let (input, kind) = opt(terminated(alt((tag("public"), tag("weak"))), space1))(input)?;
    let (input, import) = string_literal(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char(';')(input)?;
    match kind {
        None => Ok((input, Import::Standard(import))),
        Some(b"public") => Ok((input, Import::Public(import))),
        Some(b"weak") => Ok((input, Import::Weak(import))),
        Some(_) => unreachable!(),
    }
}

fn string_literal(input: &[u8]) -> IResult<&[u8], String> {
    alt((
        delimited(char('"'), char_vals, char('"')),
        delimited(char('\''), char_vals, char('\'')),
    ))(input)
}

fn char_vals(input: &[u8]) -> IResult<&[u8], String> {
    let (input, result) = many1(alt((
        hex_escaped,
        oct_escaped,
        escaped_char,
        none_of(r#"[^\0\n\\]'""#).map(|c| format!("{c}")),
    )))(input)?;
    Ok((input, result.join("")))
}

fn hex_escaped(input: &[u8]) -> IResult<&[u8], String> {
    let (output, _) = char('\\')(input)?;
    let (output, _) = alt((char('x'), char('X')))(output)?;
    let (output, _) = char_matches(is_hex_digit)(output)?;
    let (output, _) = char_matches(is_hex_digit)(output)?;
    let bytes = input[0..4].into();
    // safe because we checked all the values are valid UTF
    Ok((output, unsafe { String::from_utf8_unchecked(bytes) }))
}

fn oct_escaped(input: &[u8]) -> IResult<&[u8], String> {
    let (output, _) = char('\\')(input)?;
    let (output, _) = char_matches(is_oct_digit)(output)?;
    let (output, _) = char_matches(is_oct_digit)(output)?;
    let (output, _) = char_matches(is_oct_digit)(output)?;
    let bytes = input[0..4].into();
    // safe because we checked all the values are valid UTF
    Ok((output, unsafe { String::from_utf8_unchecked(bytes) }))
}

fn escaped_char(input: &[u8]) -> IResult<&[u8], String> {
    let (output, _) = char('\\')(input)?;
    let (output, _) = one_of(r#"abfnrtv\'""#)(output)?;
    let bytes = input[0..2].into();
    // safe because we checked all the values are valid UTF
    Ok((output, unsafe { String::from_utf8_unchecked(bytes) }))
}

pub fn char_matches<F>(cond: F) -> impl Fn(&[u8]) -> IResult<&[u8], char>
where
    F: Fn(u8) -> bool,
{
    move |i: &[u8]| match (i).iter_elements().next().map(|t| (t.as_char(), cond(t))) {
        Some((c, true)) => Ok((i.slice(c.len()..), c)),
        _ => Err(nom::Err::Error(Error::new(i, ErrorKind::Char))),
    }
}

fn syntax(input: &[u8]) -> IResult<&[u8], String> {
    let (input, _) = space0(input)?;
    let (input, _) = tag(b"syntax")(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char('=')(input)?;
    let (input, _) = space0(input)?;
    let (input, syntax) = alt((
        delimited(char('\''), tag("proto2"), char('\'')),
        delimited(char('"'), tag("proto2"), char('"')),
        delimited(char('\''), tag("proto3"), char('\'')),
        delimited(char('"'), tag("proto3"), char('"')),
    ))(input)?;
    let syntax = unsafe { String::from_utf8_unchecked(syntax.to_vec()) };
    let (input, _) = space0(input)?;
    let (input, _) = char(';')(input)?;
    Ok((input, syntax))
}

enum Constant {
    Ident(String),
    Int(i32),
    Float(f64),
    String(String),
    Bool(bool),
}

fn constant(input: &[u8]) -> IResult<&[u8], Constant> {
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

fn exponent(input: &[u8]) -> IResult<&[u8], &[u8]> {
    recognize(tuple((
        alt((char('e'), char('E'))),
        opt(alt((char('+'), char('-')))),
        digit1,
    )))(input)
}

fn int_literal(input: &[u8]) -> IResult<&[u8], i32> {
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

// Currently we're just going to eat the options since we don't really need them
fn option(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = space0(input)?;
    let (input, _) = tag(b"option")(input)?;
    let (input, _) = space1(input)?;
    let (input, _) = option_parts(input)?;
    let (input, _) = char(';')(input)?;
    Ok((input, ()))
}

fn option_parts(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = space0(input)?;
    let (input, _) = alt((ident, delimited(tag("("), full_ident, tag(b")"))))(input)?;
    let (input, _) = many0(preceded(char('.'), ident))(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char('=')(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = constant(input)?;
    let (input, _) = space0(input)?;
    Ok((input, ()))
}

fn enumeration(input: &[u8]) -> IResult<&[u8], EnumDescriptorProto> {
    let (input, _) = space0(input)?;
    let (input, _) = tag(b"enum")(input)?;
    let (input, _) = space1(input)?;
    let (input, name) = ident(input)?;
    let mut result = EnumDescriptorProto {
        name: Some(name),
        ..EnumDescriptorProto::default()
    };
    let (input, _) = space0(input)?;
    let (input, _) = char('{')(input)?;
    let mut input = input;
    loop {
        let (next, _) = many0(line_ending)(input)?;
        if let (next, Some(_)) = opt(option)(next)? {
            input = next;
            continue;
        }
        if let (next, Some(value)) = opt(enum_field)(input)? {
            result.value.push(value);
            input = next;
            continue;
        }
        if let (next, Some(_)) = opt(empty_statement)(input)? {
            input = next;
            continue;
        }
        if let (next, Some(reservation)) = opt(reserved)(input)? {
            match reservation {
                Reserved::Names(mut names) => result.reserved_name.append(&mut names),
                Reserved::Range(mut ranges) => result.reserved_range.append(&mut ranges),
            }
            input = next;
            continue;
        }
        break;
    }
    let (input, _) = char('}')(input)?;
    Ok((input, result))
}

fn enum_field(input: &[u8]) -> IResult<&[u8], EnumValueDescriptorProto> {
    let (input, _) = space0(input)?;
    let (input, name) = ident(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char('=')(input)?;
    let (input, _) = space0(input)?;
    let (input, id) = tuple((opt(char('-')), int_literal))
        .map(|(neg, num)| neg.map(|_| -num).unwrap_or(num))
        .parse(input)?;
    let (input, _) = opt(delimited(
        char('['),
        separated_list1(char(','), option_parts),
        char(']'),
    ))(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char(';')(input)?;
    Ok((
        input,
        EnumValueDescriptorProto {
            name: Some(name),
            number: Some(id),
            options: None,
        },
    ))
}

fn empty_statement(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = space0(input)?;
    let (input, _) = char(';')(input)?;
    Ok((input, ()))
}

enum Reserved {
    Range(Vec<EnumReservedRange>),
    Names(Vec<String>),
}

fn reserved(input: &[u8]) -> IResult<&[u8], Reserved> {
    let (input, _) = space0(input)?;
    let (input, _) = tag("reserved")(input)?;
    let (input, _) = space1(input)?;
    let (input, result) = alt((
        separated_list1(tuple((space0, char(','), space0)), range).map(Reserved::Range),
        separated_list1(tuple((space0, char(','), space0)), field_name).map(Reserved::Names),
    ))(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char(';')(input)?;
    Ok((input, result))
}

fn range(input: &[u8]) -> IResult<&[u8], EnumReservedRange> {
    let (input, _) = space0(input)?;
    let (input, start) = int_literal(input)?;
    let (input, end) = opt(preceded(
        tuple((space1, tag("to"), space1)),
        alt((int_literal, tag("max").map(|_| i32::MAX))),
    ))(input)?;
    Ok((
        input,
        EnumReservedRange {
            start: Some(start),
            end,
        },
    ))
}

fn field_name(input: &[u8]) -> IResult<&[u8], String> {
    let (input, _) = space0(input)?;
    let (input, name) = alt((
        delimited(char('"'), ident, char('"')),
        delimited(char('\''), ident, char('\'')),
    ))(input)?;
    let (input, _) = space0(input)?;
    Ok((input, name))
}

enum MessagePart {
    Blank,
    Enum(EnumDescriptorProto),
    Field(FieldDescriptorProto),
    MapField((DescriptorProto, FieldDescriptorProto)),
    Message(DescriptorProto),
    OneOf((OneofDescriptorProto, Vec<FieldDescriptorProto>)),
    Reserved(Reserved),
}

fn message(proto_v2: bool) -> impl Fn(&[u8]) -> IResult<&[u8], DescriptorProto> {
    move |input: &[u8]| {
        let (input, _) = space0(input)?;
        let (input, _) = tag("message")(input)?;
        let (input, _) = space1(input)?;
        let (input, name) = ident(input)?;
        let (input, _) = space0(input)?;
        let mut result = DescriptorProto {
            name: Some(name),
            ..DescriptorProto::default()
        };
        let (input, parts) = delimited(
            char('{'),
            many0(alt((
                line_ending.map(|_| MessagePart::Blank),
                field(proto_v2).map(MessagePart::Field),
                enumeration.map(MessagePart::Enum),
                message(proto_v2).map(MessagePart::Message),
                // TODO extend proto2 only
                // TODO extention proto2 only
                // TODO group proto2 only
                option.map(|_| MessagePart::Blank),
                oneof_field.map(MessagePart::OneOf),
                map_field.map(MessagePart::MapField),
                reserved.map(MessagePart::Reserved),
                empty_statement.map(|_| MessagePart::Blank),
            ))),
            char('}'),
        )(input)?;
        for part in parts {
            match part {
                MessagePart::Blank => (),
                MessagePart::Enum(enum_type) => result.enum_type.push(enum_type),
                MessagePart::Field(field) => result.field.push(field),
                MessagePart::MapField((message, field)) => {
                    result.nested_type.push(message);
                    result.field.push(field);
                }
                MessagePart::Message(nested_type) => result.nested_type.push(nested_type),
                MessagePart::OneOf((oneof, fields)) => {
                    let index = result.oneof_decl.len().try_into().unwrap();
                    result.oneof_decl.push(oneof);
                    for mut field in fields {
                        field.oneof_index = Some(index);
                        result.field.push(field);
                    }
                }
                MessagePart::Reserved(Reserved::Names(mut names)) => {
                    result.reserved_name.append(&mut names)
                }
                MessagePart::Reserved(Reserved::Range(ranges)) => {
                    for range in ranges {
                        result.reserved_range.push(ReservedRange {
                            start: range.start,
                            end: range.end,
                        })
                    }
                }
            }
        }
        Ok((input, result))
    }
}

fn map_field(input: &[u8]) -> IResult<&[u8], (DescriptorProto, FieldDescriptorProto)> {
    let (input, _) = space0(input)?;
    let (input, _) = tag("map")(input)?;
    let (input, _) = char('<')(input)?;
    let (input, _) = space0(input)?;
    let (input, kt) = key_type(input)?;
    let (input, _) = char(',')(input)?;
    let (input, _) = space0(input)?;
    let (input, (vn, vt)) = field_type(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char('>')(input)?;
    let (input, _) = space0(input)?;
    let (input, name) = ident(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char('=')(input)?;
    let (input, _) = space0(input)?;
    let (input, number) = int_literal(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = opt(delimited(
        char('['),
        separated_list1(char(','), option_parts),
        char(']'),
    ))(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char(';')(input)?;

    let message_name = format!("{}Entry", name.to_case(Case::UpperCamel));
    let mut message = DescriptorProto {
        name: Some(message_name.clone()),
        options: Some(MessageOptions {
            map_entry: Some(true),
            ..MessageOptions::default()
        }),
        ..DescriptorProto::default()
    };
    message.field.push(FieldDescriptorProto {
        name: Some(String::from("key")),
        number: Some(1),
        label: Some(1),
        r#type: Some(kt),
        ..FieldDescriptorProto::default()
    });
    message.field.push(FieldDescriptorProto {
        name: Some(String::from("value")),
        number: Some(2),
        label: Some(1),
        r#type: Some(vt),
        type_name: vn,
        ..FieldDescriptorProto::default()
    });
    Ok((
        input,
        (
            message,
            FieldDescriptorProto {
                name: Some(name),
                number: Some(number),
                // repeated field
                label: Some(3),
                // embedded message type
                r#type: Some(11),
                type_name: Some(message_name),
                ..FieldDescriptorProto::default()
            },
        ),
    ))
}

fn key_type(input: &[u8]) -> IResult<&[u8], i32> {
    alt((
        tag("int64").map(|_| 3),
        tag("uint64").map(|_| 4),
        tag("int32").map(|_| 5),
        tag("fixed64").map(|_| 6),
        tag("fixed32").map(|_| 7),
        tag("bool").map(|_| 8),
        tag("string").map(|_| 9),
        tag("uint32").map(|_| 13),
        tag("sfixed32").map(|_| 15),
        tag("sfixed64").map(|_| 16),
        tag("sint32").map(|_| 17),
        tag("sint64").map(|_| 18),
    ))(input)
}

fn field(proto_v2: bool) -> impl Fn(&[u8]) -> IResult<&[u8], FieldDescriptorProto> {
    move |input: &[u8]| {
        let (input, _) = space0(input)?;
        let (input, label) = if proto_v2 {
            let (input, label) = alt((
                tag("optional").map(|_| Some(1)),
                tag("required").map(|_| Some(2)),
                tag("repeated").map(|_| Some(3)),
            ))(input)?;
            let (input, _) = space1(input)?;
            (input, label)
        } else {
            let (input, label) = opt(tag("repeated"))(input)?;
            let (input, _) = space0(input)?;
            (input, label.map(|_| 3))
        };
        let (input, mut result) = basic_field(input)?;
        result.label = label;
        Ok((input, result))
    }
}

fn basic_field(input: &[u8]) -> IResult<&[u8], FieldDescriptorProto> {
    let (input, (type_name, r#type)) = field_type(input)?;

    let (input, _) = space1(input)?;
    let (input, name) = ident(input)?;

    let (input, _) = space0(input)?;
    let (input, _) = char('=')(input)?;

    let (input, _) = space0(input)?;
    let (input, number) = int_literal(input)?;

    let (input, _) = space0(input)?;
    let (input, _) = opt(delimited(
        char('['),
        separated_list1(char(','), option_parts),
        char(']'),
    ))(input)?;

    let (input, _) = space0(input)?;
    let (input, _) = char(';')(input)?;
    Ok((
        input,
        FieldDescriptorProto {
            name: Some(name),
            number: Some(number),
            r#type: Some(r#type),
            type_name,
            ..FieldDescriptorProto::default()
        },
    ))
}

fn field_type(input: &[u8]) -> IResult<&[u8], (Option<String>, i32)> {
    let (input, _) = space0(input)?;
    alt((
        tag("double").map(|_| (None, 1)),
        tag("float").map(|_| (None, 2)),
        tag("int64").map(|_| (None, 3)),
        tag("uint64").map(|_| (None, 4)),
        tag("int32").map(|_| (None, 5)),
        tag("fixed64").map(|_| (None, 6)),
        tag("fixed32").map(|_| (None, 7)),
        tag("bool").map(|_| (None, 8)),
        tag("string").map(|_| (None, 9)),
        // group = 10
        message_type.map(|name| (Some(name), 11)),
        tag("bytes").map(|_| (None, 12)),
        tag("uint32").map(|_| (None, 13)),
        // TODO enum = 14; is there something that we need to do?
        tag("sfixed32").map(|_| (None, 15)),
        tag("sfixed64").map(|_| (None, 16)),
        tag("sint32").map(|_| (None, 17)),
        tag("sint64").map(|_| (None, 18)),
    ))(input)
}

fn message_type(input: &[u8]) -> IResult<&[u8], String> {
    let (input, value) =
        recognize(tuple((opt(char('.')), separated_list1(char('.'), ident))))(input)?;
    let mt = unsafe { String::from_utf8_unchecked(value.to_vec()) };
    Ok((input, mt))
}

fn oneof_field(input: &[u8]) -> IResult<&[u8], (OneofDescriptorProto, Vec<FieldDescriptorProto>)> {
    let (input, _) = space0(input)?;
    let (input, _) = tag("oneof")(input)?;
    let (input, _) = space1(input)?;
    let (input, name) = ident(input)?;
    let (input, _) = space0(input)?;
    let (input, fields) = delimited(
        char('{'),
        many0(alt((
            line_ending.map(|_| None),
            option.map(|_| None),
            empty_statement.map(|_| None),
            basic_field.map(Some),
        ))),
        char('}'),
    )(input)?;
    let fields = fields.iter().filter_map(|v| v.to_owned()).collect();
    Ok((
        input,
        (
            OneofDescriptorProto {
                name: Some(name),
                options: None,
            },
            fields,
        ),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn service_test() {
        let input = b"service SmokeService {\n\n    // A detached comment block.\n\n    // Blow some smoke.\n    rpc BlowSmoke(SmokeRequest) returns (SmokeResponse);\n}";
        let (remaining, service) = service(input).unwrap();
        assert_eq!(&b""[..], remaining);
        assert_eq!(
            ServiceDescriptorProto {
                name: Some(String::from("SmokeService")),
                options: None,
                method: vec![MethodDescriptorProto {
                    name: Some(String::from("BlowSmoke")),
                    input_type: Some(String::from("SmokeRequest")),
                    output_type: Some(String::from("SmokeResponse")),
                    ..MethodDescriptorProto::default()
                }],
            },
            service
        );
    }

    #[test]
    fn oneof_test() {
        let input = b"oneof foo {\n\tstring name = 4;\n\tSubMessage sub_message = 9;\n}";
        let (remaining, (desc, fields)) = oneof_field(input).unwrap();
        assert_eq!(&b""[..], remaining);
        assert_eq!(
            desc,
            OneofDescriptorProto {
                name: Some(String::from("foo")),
                options: None,
            }
        );
        assert_eq!(
            fields,
            vec![
                FieldDescriptorProto {
                    name: Some(String::from("name")),
                    number: Some(4),
                    r#type: Some(9),
                    ..FieldDescriptorProto::default()
                },
                FieldDescriptorProto {
                    name: Some(String::from("sub_message")),
                    number: Some(9),
                    r#type: Some(11),
                    type_name: Some(String::from("SubMessage")),
                    ..FieldDescriptorProto::default()
                },
            ]
        );
    }

    #[test]
    fn field_test() {
        assert_eq!(
            field(true)(b"optional foo.bar nested_message = 2;"),
            Ok((
                &b""[..],
                FieldDescriptorProto {
                    name: Some(String::from("nested_message")),
                    number: Some(2),
                    label: Some(1),
                    r#type: Some(11),
                    type_name: Some(String::from("foo.bar")),
                    ..FieldDescriptorProto::default()
                }
            ))
        );
        assert_eq!(
            field(true)(b"repeated int32 samples = 4 [packed=true];"),
            Ok((
                &b""[..],
                FieldDescriptorProto {
                    name: Some(String::from("samples")),
                    number: Some(4),
                    label: Some(3),
                    r#type: Some(5),
                    ..FieldDescriptorProto::default()
                }
            ))
        );
        assert_eq!(
            field(false)(b"foo.Bar nested_message = 2;"),
            Ok((
                &b""[..],
                FieldDescriptorProto {
                    name: Some(String::from("nested_message")),
                    number: Some(2),
                    r#type: Some(11),
                    type_name: Some(String::from("foo.Bar")),
                    ..FieldDescriptorProto::default()
                }
            ))
        );
        assert_eq!(
            field(false)(b"repeated int32 samples = 4 [packed=true];"),
            Ok((
                &b""[..],
                FieldDescriptorProto {
                    name: Some(String::from("samples")),
                    number: Some(4),
                    label: Some(3),
                    r#type: Some(5),
                    ..FieldDescriptorProto::default()
                }
            ))
        );
    }

    #[test]
    fn syntax_test() {
        assert_eq!(
            syntax(b"syntax = \"proto2\";"),
            Ok((&b""[..], String::from("proto2")))
        );
        assert_eq!(
            syntax(b"\t\tsyntax  =  'proto2' ;"),
            Ok((&b""[..], String::from("proto2")))
        );
        assert_eq!(
            syntax(b"syntax=\"proto3\";"),
            Ok((&b""[..], String::from("proto3")))
        );
        assert_eq!(
            syntax(b"syntax = 'proto3';"),
            Ok((&b""[..], String::from("proto3")))
        );
    }

    #[test]
    fn import_test() {
        assert_eq!(
            import(b"  import 'types.proto' ;"),
            Ok((&b""[..], Import::Standard("types.proto".into())))
        );

        assert_eq!(
            import(b"import public \"types.proto\" ;"),
            Ok((&b""[..], Import::Public("types.proto".into())))
        );

        assert_eq!(
            import(b"import\tweak  \"types.proto\";"),
            Ok((&b""[..], Import::Weak("types.proto".into())))
        );
    }

    #[test]
    fn package_test() {
        assert_eq!(
            package(b"package foo.bar;"),
            Ok((&b""[..], String::from("foo.bar"))),
        );
        assert_eq!(
            package(b"  package \t foo.bar  ;"),
            Ok((&b""[..], String::from("foo.bar"))),
        );
    }

    #[test]
    fn option_test() {
        assert_eq!(
            option(b"option java_package = \"com.example.foo\";"),
            Ok((&b""[..], ())),
        );
        assert_eq!(
            option(b"option\tjava_package = \"(com.example).foo\"  ;"),
            Ok((&b""[..], ())),
        );
    }
}
