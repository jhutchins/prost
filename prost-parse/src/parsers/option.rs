use super::lexical;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, space0, space1},
    multi::many0,
    sequence::{delimited, preceded},
    IResult,
};

/// Parse a protobuf [option][0]
///
/// ```ebnf
/// option = "option" optionName  "=" constant ";"
/// optionName = ( ident | "(" fullIdent ")" ) { "." ident }
/// ```
///
/// Currently we're just going to ignore the options since we don't use them.
///
/// [0]: https://developers.google.com/protocol-buffers/docs/reference/proto3-spec#option
pub fn parser(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = space0(input)?;
    let (input, _) = tag(b"option")(input)?;
    let (input, _) = space1(input)?;
    let (input, _) = parts(input)?;
    let (input, _) = char(';')(input)?;
    Ok((input, ()))
}

/// Helper parse function for a portion of the option statement useful in multiple parsers.
pub fn parts(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = space0(input)?;
    let (input, _) = alt((
        lexical::ident,
        delimited(tag("("), lexical::full_ident, tag(b")")),
    ))(input)?;
    let (input, _) = many0(preceded(char('.'), lexical::ident))(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char('=')(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = lexical::constant(input)?;
    let (input, _) = space0(input)?;
    Ok((input, ()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parser_test() {
        assert_eq!(
            parser(b"option java_package = \"com.example.foo\";"),
            Ok((&b""[..], ())),
        );
        assert_eq!(
            parser(b"option\tjava_package = \"(com.example).foo\"  ;"),
            Ok((&b""[..], ())),
        );
    }
}
