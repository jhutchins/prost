use super::{lexical, option, reserved};
use nom::{
    bytes::complete::tag,
    character::complete::{char, line_ending, space0, space1},
    combinator::opt,
    multi::{many0, separated_list1},
    sequence::{delimited, tuple},
    IResult, Parser,
};
use prost_types::{EnumDescriptorProto, EnumValueDescriptorProto};

/// Parse a protobuf [enum][0]
///
/// ```ebnf
/// enum = "enum" enumName enumBody
/// enumBody = "{" { option | enumField | emptyStatement | reserved } "}"
/// enumField = ident "=" [ "-" ] intLit [ "[" enumValueOption { ","  enumValueOption } "]" ]";"
/// enumValueOption = optionName "=" constant
/// ```
///
/// [0]: https://developers.google.com/protocol-buffers/docs/reference/proto3-spec#enum_definition
pub fn parser(input: &[u8]) -> IResult<&[u8], EnumDescriptorProto> {
    let (input, _) = space0(input)?;
    let (input, _) = tag(b"enum")(input)?;
    let (input, _) = space1(input)?;
    let (input, name) = lexical::ident(input)?;
    let mut result = EnumDescriptorProto {
        name: Some(name),
        ..EnumDescriptorProto::default()
    };
    let (input, _) = space0(input)?;
    let (input, _) = char('{')(input)?;
    let mut input = input;
    loop {
        let (next, _) = many0(line_ending)(input)?;
        if let (next, Some(_)) = opt(option::parser)(next)? {
            input = next;
            continue;
        }
        if let (next, Some(value)) = opt(field)(input)? {
            result.value.push(value);
            input = next;
            continue;
        }
        if let (next, Some(_)) = opt(lexical::empty_statement)(input)? {
            input = next;
            continue;
        }
        if let (next, Some(reservation)) = opt(reserved::parser)(input)? {
            match reservation {
                reserved::Type::Names(mut names) => result.reserved_name.append(&mut names),
                reserved::Type::Range(mut ranges) => result.reserved_range.append(&mut ranges),
            }
            input = next;
            continue;
        }
        break;
    }
    let (input, _) = char('}')(input)?;
    Ok((input, result))
}

/// Parse a protobuf [enumField][0]
///
/// ```ebnf
/// enumField = ident "=" [ "-" ] intLit [ "[" enumValueOption { ","  enumValueOption } "]" ]";"
/// ```
///
/// [0]: https://developers.google.com/protocol-buffers/docs/reference/proto3-spec#enum_definition
fn field(input: &[u8]) -> IResult<&[u8], EnumValueDescriptorProto> {
    let (input, _) = space0(input)?;
    let (input, name) = lexical::ident(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char('=')(input)?;
    let (input, _) = space0(input)?;
    let (input, id) = tuple((opt(char('-')), lexical::int_literal))
        .map(|(neg, num)| neg.map(|_| -num).unwrap_or(num))
        .parse(input)?;
    let (input, _) = opt(delimited(
        char('['),
        separated_list1(char(','), option::parts),
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
