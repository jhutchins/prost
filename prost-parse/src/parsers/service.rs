use super::{comment, lexical, option};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, multispace1, space0, space1},
    combinator::opt,
    multi::{many0, separated_list1},
    sequence::{delimited, preceded, terminated, tuple},
    IResult, Parser,
};
use prost_types::{MethodDescriptorProto, ServiceDescriptorProto};

/// Parse a protobuf [service][0]
///
/// ```ebnf
/// service = "service" serviceName "{" { option | rpc | emptyStatement } "}"
/// rpc = "rpc" rpcName "(" [ "stream" ] messageType ")" "returns" "(" [ "stream" ]
/// messageType ")" (( "{" { option | emptyStatement } "}" ) | ";")
/// ```
///
/// This does not currently support parsing [stream][1] entries which is a proto2 concept defined as
///
/// ```ebnf
/// stream = "stream" streamName "(" messageType "," messageType ")" (( "{"
/// { option | emptyStatement } "}") | ";" )
/// ```
///
/// Having a stream defined on your service will cause the parser to fail.
///
/// [0]: https://developers.google.com/protocol-buffers/docs/reference/proto3-spec#service_definition
/// [1]: https://developers.google.com/protocol-buffers/docs/reference/proto2-spec#service_definition
pub fn parser(input: &[u8]) -> IResult<&[u8], ServiceDescriptorProto> {
    let (input, _) = space0(input)?;
    let (input, _) = tag("service")(input)?;
    let (input, _) = space1(input)?;
    let (input, name) = lexical::ident(input)?;
    let (input, _) = space0(input)?;
    let (input, method) = delimited(
        char('{'),
        many0(alt((
            option::parser.map(|_| None),
            lexical::empty_statement.map(|_| None),
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

/// Parse a protobuf [rpc][0]
///
/// ```ebnf
/// rpc = "rpc" rpcName "(" [ "stream" ] messageType ")" "returns" "(" [ "stream" ]
/// messageType ")" (( "{" { option | emptyStatement } "}" ) | ";")
/// ```
///
/// [0]: https://developers.google.com/protocol-buffers/docs/reference/proto3-spec#service_definition
fn rpc(input: &[u8]) -> IResult<&[u8], MethodDescriptorProto> {
    let (input, _) = space0(input)?;
    let (input, _) = tag("rpc")(input)?;
    let (input, _) = space1(input)?;
    let (input, name) = lexical::ident.map(Some).parse(input)?;
    let (input, _) = space0(input)?;
    let (input, (client_streaming, input_type)) = delimited(
        terminated(char('('), space0),
        tuple((
            opt(terminated(tag("stream").map(|_| true), space1)),
            lexical::message_type.map(Some),
        )),
        preceded(space0, char(')')),
    )(input)?;
    let (input, _) = delimited(space0, tag("returns"), space0)(input)?;
    let (input, (server_streaming, output_type)) = delimited(
        terminated(char('('), space0),
        tuple((
            opt(terminated(tag("stream").map(|_| true), space1)),
            lexical::message_type.map(Some),
        )),
        preceded(space0, char(')')),
    )(input)?;
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parser_test() {
        let input = b"service SmokeService {\n\n    // A detached comment block.\n\n    // Blow some smoke.\n    rpc BlowSmoke(SmokeRequest) returns (SmokeResponse);\n}";
        let (remaining, service) = parser(input).unwrap();
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
}
