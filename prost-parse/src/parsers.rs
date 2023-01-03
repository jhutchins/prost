use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{char, line_ending, not_line_ending, space0, space1},
    combinator::opt,
    multi::many0,
    sequence::{delimited, preceded, terminated},
    IResult,
};
use prost_types::FileDescriptorProto;
use std::io::{self, Result};

mod enumeration;
mod fields;
mod lexical;
mod message;
mod option;
mod reserved;
mod service;

/// Parse the contents of an entire protobuf file
pub fn parse(input: Vec<u8>) -> Result<FileDescriptorProto> {
    match proto_file(&input) {
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

/// Represent that various kinds of import statements, public, standard, or weak
#[derive(Debug, Eq, PartialEq)]
enum Import {
    Public(String),
    Standard(String),
    Weak(String),
}

/// Parse an entire contexts of a protocol buffer file
///
/// Definition[0]
/// ```ebnf
/// proto = syntax { import | package | option | topLevelDef | emptyStatement }
/// topLevelDef = message | enum | extend | service
/// ```
///
/// [0]: https://developers.google.com/protocol-buffers/docs/reference/proto2-spec#proto_file
fn proto_file(input: &[u8]) -> IResult<&[u8], FileDescriptorProto> {
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
        if let (next, Some(())) = opt(option::parser)(next)? {
            input = next;
            continue;
        }
        if let (next, Some(enum_type)) = opt(enumeration::parser)(next)? {
            result.enum_type.push(enum_type);
            input = next;
            continue;
        }
        if let (next, Some(message_type)) = opt(message::parser(&syntax == "proto2"))(next)? {
            result.message_type.push(message_type);
            input = next;
            continue;
        }
        if let (next, Some(_)) = opt(comment)(next)? {
            input = next;
            continue;
        }
        if let (next, Some(service)) = opt(service::parser)(next)? {
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

/// Parse protobuf comments; which are not defined as part of the langauge
/// grammer, but are defined the same for both [proto2][0] and [proto3][1] as
/// C/C++ style comments using `//` and `/*...*/` syntax.
///
/// Currently the comments are not captured in any way and are simply ignored.
///
/// [0]: https://developers.google.com/protocol-buffers/docs/proto#adding-comments
/// [1]: https://developers.google.com/protocol-buffers/docs/proto3#adding_comments
fn comment(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = alt((
        preceded(tag("//"), not_line_ending),
        delimited(tag("/*"), take_until("*/"), tag("*/")),
    ))(input)?;
    Ok((input, ()))
}

/// Parse a protobuf [import][0]
///
/// ```ebnf
/// import = "import" [ "weak" | "public" ] strLit ";"
/// ```
///
/// [0]: https://developers.google.com/protocol-buffers/docs/reference/proto3-spec#import_statement
fn import(input: &[u8]) -> IResult<&[u8], Import> {
    let (input, _) = space0(input)?;
    let (input, _) = tag("import")(input)?;
    let (input, _) = space1(input)?;
    let (input, kind) = opt(terminated(alt((tag("public"), tag("weak"))), space1))(input)?;
    let (input, import) = lexical::string_literal(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char(';')(input)?;
    match kind {
        None => Ok((input, Import::Standard(import))),
        Some(b"public") => Ok((input, Import::Public(import))),
        Some(b"weak") => Ok((input, Import::Weak(import))),
        Some(_) => unreachable!(),
    }
}

/// Parse a protobuf [package][0]
///
/// ```ebnf
/// package = "package" fullIdent ";"
/// ```
///
/// [0]: https://developers.google.com/protocol-buffers/docs/reference/proto3-spec#package
fn package(input: &[u8]) -> IResult<&[u8], String> {
    let (input, _) = space0(input)?;
    let (input, _) = tag("package")(input)?;
    let (input, _) = space1(input)?;
    let (input, name) = lexical::full_ident(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char(';')(input)?;
    Ok((input, name))
}

/// Parse a protobuf syntax statement
///
/// [proto2][0]
/// ```ebnf
/// syntax = "syntax" "=" ("'" "proto2" "'" | '"' "proto2" '"') ";"
/// ```
///
/// [proto3][1]
/// ```ebnf
/// syntax = "syntax" "=" ("'" "proto3" "'" | '"' "proto3" '"') ";"
/// ```
///
/// [0]: https://developers.google.com/protocol-buffers/docs/reference/proto2-spec#syntax
/// [1]: https://developers.google.com/protocol-buffers/docs/reference/proto3-spec#syntax
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

#[cfg(test)]
mod tests {
    use super::*;

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
}
