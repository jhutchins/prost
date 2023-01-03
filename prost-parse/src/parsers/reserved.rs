use super::lexical;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, space0, space1},
    combinator::opt,
    multi::separated_list1,
    sequence::{delimited, preceded, tuple},
    IResult, Parser,
};
use prost_types::enum_descriptor_proto::EnumReservedRange;

/// Represents the different types of reserved statements.
///
/// A resevered statement declares a range of field numbers or field names that
/// cannot be used in a protobuf message.
///
/// [Range][Reserved::Range] is a collection of reserved field numbers
/// [Names][Reserved::Names] is a collection of reserved names
pub enum Type {
    Range(Vec<EnumReservedRange>),
    Names(Vec<String>),
}

/// Parse a protobuf [reserved][0]
///
/// ```ebnf
/// reserved = "reserved" ( ranges | strFieldNames ) ";"
/// ranges = range { "," range }
/// range =  intLit [ "to" ( intLit | "max" ) ]
/// strFieldNames = strFieldName { "," strFieldName }
/// strFieldName = "'" fieldName "'" | '"' fieldName '"'
///
/// ```
///
/// [0]: https://developers.google.com/protocol-buffers/docs/reference/proto3-spec#reserved
pub fn parser(input: &[u8]) -> IResult<&[u8], Type> {
    let (input, _) = space0(input)?;
    let (input, _) = tag("reserved")(input)?;
    let (input, _) = space1(input)?;
    let (input, result) = alt((
        separated_list1(tuple((space0, char(','), space0)), range).map(Type::Range),
        separated_list1(tuple((space0, char(','), space0)), field_name).map(Type::Names),
    ))(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char(';')(input)?;
    Ok((input, result))
}

/// Parse a protobuf [range][0]
///
/// ```ebnf
/// range =  intLit [ "to" ( intLit | "max" ) ]
///
/// ```
///
/// [0]: https://developers.google.com/protocol-buffers/docs/reference/proto3-spec#reserved
fn range(input: &[u8]) -> IResult<&[u8], EnumReservedRange> {
    let (input, _) = space0(input)?;
    let (input, start) = lexical::int_literal(input)?;
    let (input, end) = opt(preceded(
        tuple((space1, tag("to"), space1)),
        alt((lexical::int_literal, tag("max").map(|_| i32::MAX))),
    ))(input)?;
    Ok((
        input,
        EnumReservedRange {
            start: Some(start),
            end,
        },
    ))
}

/// Parse a protobuf [strFieldName][0]
///
/// ```ebnf
/// strFieldName = "'" fieldName "'" | '"' fieldName '"'
/// ```
///
/// [0]: https://developers.google.com/protocol-buffers/docs/reference/proto3-spec#reserved
fn field_name(input: &[u8]) -> IResult<&[u8], String> {
    let (input, _) = space0(input)?;
    let (input, name) = alt((
        delimited(char('"'), lexical::ident, char('"')),
        delimited(char('\''), lexical::ident, char('\'')),
    ))(input)?;
    let (input, _) = space0(input)?;
    Ok((input, name))
}
