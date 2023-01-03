use super::{lexical, option};
use convert_case::{Case, Casing};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, line_ending, space0, space1},
    combinator::opt,
    multi::{many0, separated_list1},
    sequence::delimited,
    IResult, Parser,
};
use prost_types::{DescriptorProto, FieldDescriptorProto, MessageOptions, OneofDescriptorProto};

/// Parse a protobuf [mapField][0]
///
/// ```ebnf
/// mapField = "map" "<" keyType "," type ">" mapName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
/// keyType = "int32" | "int64" | "uint32" | "uint64" | "sint32" | "sint64" |
///           "fixed32" | "fixed64" | "sfixed32" | "sfixed64" | "bool" | "string"
/// ```
///
/// [0]: https://developers.google.com/protocol-buffers/docs/reference/proto2-spec#map_field
pub fn map(input: &[u8]) -> IResult<&[u8], (DescriptorProto, FieldDescriptorProto)> {
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
    let (input, name) = lexical::ident(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char('=')(input)?;
    let (input, _) = space0(input)?;
    let (input, number) = lexical::int_literal(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = opt(delimited(
        char('['),
        separated_list1(char(','), option::parts),
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

/// Generate a parser for either a proto2 or proto3 style normal field declaration
///
/// [proto2][0]
/// ```ebnf
/// label = "required" | "optional" | "repeated"
/// type = "double" | "float" | "int32" | "int64" | "uint32" | "uint64"
///       | "sint32" | "sint64" | "fixed32" | "fixed64" | "sfixed32" | "sfixed64"
///       | "bool" | "string" | "bytes" | messageType | enumType
/// fieldNumber = intLit;
///
/// field = label type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
/// fieldOptions = fieldOption { ","  fieldOption }
/// fieldOption = optionName "=" constant
/// ```
///
/// [proto3][1]
/// ```ebnf
/// type = "double" | "float" | "int32" | "int64" | "uint32" | "uint64"
///       | "sint32" | "sint64" | "fixed32" | "fixed64" | "sfixed32" | "sfixed64"
///       | "bool" | "string" | "bytes" | messageType | enumType
/// fieldNumber = intLit;
///
/// field = [ "repeated" ] type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
/// fieldOptions = fieldOption { ","  fieldOption }
/// fieldOption = optionName "=" constant
/// ```
///
/// [0]: https://developers.google.com/protocol-buffers/docs/reference/proto2-spec#fields
/// [1]: https://developers.google.com/protocol-buffers/docs/reference/proto3-spec#fields
pub fn normal(proto_v2: bool) -> impl Fn(&[u8]) -> IResult<&[u8], FieldDescriptorProto> {
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

/// Parse a protobuf [oneof][0]
///
/// ```ebnf
/// oneof = "oneof" oneofName "{" { option | oneofField | emptyStatement } "}"
/// oneofField = type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
/// ```
/// [0]: https://developers.google.com/protocol-buffers/docs/reference/proto3-spec#oneof_and_oneof_field
pub fn oneof(input: &[u8]) -> IResult<&[u8], (OneofDescriptorProto, Vec<FieldDescriptorProto>)> {
    let (input, _) = space0(input)?;
    let (input, _) = tag("oneof")(input)?;
    let (input, _) = space1(input)?;
    let (input, name) = lexical::ident(input)?;
    let (input, _) = space0(input)?;
    let (input, fields) = delimited(
        char('{'),
        many0(alt((
            line_ending.map(|_| None),
            option::parser.map(|_| None),
            lexical::empty_statement.map(|_| None),
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

/// Parser function for a portion of the field definition used in multiple parsers
fn basic_field(input: &[u8]) -> IResult<&[u8], FieldDescriptorProto> {
    let (input, (type_name, r#type)) = field_type(input)?;

    let (input, _) = space1(input)?;
    let (input, name) = lexical::ident(input)?;

    let (input, _) = space0(input)?;
    let (input, _) = char('=')(input)?;

    let (input, _) = space0(input)?;
    let (input, number) = lexical::int_literal(input)?;

    let (input, _) = space0(input)?;
    let (input, _) = opt(delimited(
        char('['),
        separated_list1(char(','), option::parts),
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

/// Parse a protobuf [type][0]
///
/// ```ebnf
/// type = "double" | "float" | "int32" | "int64" | "uint32" | "uint64"
///       | "sint32" | "sint64" | "fixed32" | "fixed64" | "sfixed32" | "sfixed64"
///       | "bool" | "string" | "bytes" | messageType | enumType
/// ```
///
/// [0]: https://developers.google.com/protocol-buffers/docs/reference/proto3-spec#fields
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
        lexical::message_type.map(|name| (Some(name), 11)),
        tag("bytes").map(|_| (None, 12)),
        tag("uint32").map(|_| (None, 13)),
        // TODO enum = 14; is there something that we need to do?
        tag("sfixed32").map(|_| (None, 15)),
        tag("sfixed64").map(|_| (None, 16)),
        tag("sint32").map(|_| (None, 17)),
        tag("sint64").map(|_| (None, 18)),
    ))(input)
}

/// Parse a protobuf [keyType][0]
///
/// ```ebnf
/// keyType = "int32" | "int64" | "uint32" | "uint64" | "sint32" | "sint64" |
///           "fixed32" | "fixed64" | "sfixed32" | "sfixed64" | "bool" | "string"
/// ```
///
/// [0]: https://developers.google.com/protocol-buffers/docs/reference/proto2-spec#map_field
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn oneof_test() {
        let input = b"oneof foo {\n\tstring name = 4;\n\tSubMessage sub_message = 9;\n}";
        let (remaining, (desc, fields)) = oneof(input).unwrap();
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
    fn normal_test() {
        assert_eq!(
            normal(true)(b"optional foo.bar nested_message = 2;"),
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
            normal(true)(b"repeated int32 samples = 4 [packed=true];"),
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
            normal(false)(b"foo.Bar nested_message = 2;"),
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
            normal(false)(b"repeated int32 samples = 4 [packed=true];"),
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
}
