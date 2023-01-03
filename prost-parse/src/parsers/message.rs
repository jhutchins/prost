use super::{enumeration, fields, lexical, option, reserved};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, line_ending, space0, space1},
    multi::many0,
    sequence::delimited,
    IResult, Parser,
};
use prost_types::{
    descriptor_proto::ReservedRange, DescriptorProto, EnumDescriptorProto, FieldDescriptorProto,
    OneofDescriptorProto,
};

/// Parse a protobuf [message][0]
///
/// ```ebnf
/// message = "message" messageName messageBody
/// messageBody = "{" { field | enum | message | extend | extensions | group |
/// option | oneof | mapField | reserved | emptyStatement } "}"
/// ```
///
/// [0]: https://developers.google.com/protocol-buffers/docs/reference/proto2-spec#message_definition
pub fn parser(proto_v2: bool) -> impl Fn(&[u8]) -> IResult<&[u8], DescriptorProto> {
    move |input: &[u8]| {
        let (input, _) = space0(input)?;
        let (input, _) = tag("message")(input)?;
        let (input, _) = space1(input)?;
        let (input, name) = lexical::ident(input)?;
        let (input, _) = space0(input)?;
        let mut result = DescriptorProto {
            name: Some(name),
            ..DescriptorProto::default()
        };
        let (input, parts) = delimited(
            char('{'),
            many0(alt((
                line_ending.map(|_| MessagePart::Blank),
                fields::normal(proto_v2).map(MessagePart::Field),
                enumeration::parser.map(MessagePart::Enum),
                parser(proto_v2).map(MessagePart::Message),
                // TODO extend proto2 only
                // TODO extention proto2 only
                // TODO group proto2 only
                option::parser.map(|_| MessagePart::Blank),
                fields::oneof.map(MessagePart::OneOf),
                fields::map.map(MessagePart::MapField),
                reserved::parser.map(MessagePart::Reserved),
                lexical::empty_statement.map(|_| MessagePart::Blank),
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
                MessagePart::Reserved(reserved::Type::Names(mut names)) => {
                    result.reserved_name.append(&mut names)
                }
                MessagePart::Reserved(reserved::Type::Range(ranges)) => {
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

/// Represent the various unordered parts of a protobuf message
enum MessagePart {
    Blank,
    Enum(EnumDescriptorProto),
    Field(FieldDescriptorProto),
    MapField((DescriptorProto, FieldDescriptorProto)),
    Message(DescriptorProto),
    OneOf((OneofDescriptorProto, Vec<FieldDescriptorProto>)),
    Reserved(reserved::Type),
}
