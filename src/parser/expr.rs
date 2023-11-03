use super::string_parser::StringParser;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::{self, complete::one_of},
    combinator::{map, map_res, recognize},
    error::VerboseError,
    multi::{many0, many1},
    number::complete::double,
    sequence::{preceded, terminated},
    IResult, Parser,
};
//未附加属性的表达式
#[derive(Debug, PartialEq)]
pub enum Expr {
    Literal(BuiltInType),

    Ref(String),
    //暂时猜的是在Assign的过程中如果检查到符号表里没有就插入
    // 声明语句来实现声明+赋值
    Assign(String, Box<Expr>),
    //属性后置 形如 let a : int;
    VarDeclaration(String, String),

    Function(String, Vec<Box<Expr>>),

    Block(CodeBlock),

    IfBlock(Box<Expr>, CodeBlock),

    IfelseBlock(Box<Expr>, CodeBlock, CodeBlock),

    WhileBlock(Box<Expr>, CodeBlock),

    Operator(BuiltInOperator),
}
#[derive(Debug, PartialEq)]
pub enum CodeBlock {
    Code(Vec<Box<Expr>>),

    ReturnCode(Box<Expr>),
}
#[derive(Debug, PartialEq)]
// 连接运算符
pub enum BuiltInOperator {
    /// `+`
    Add(Box<Expr>, Box<Expr>),
    /// `-`
    Sub(Box<Expr>, Box<Expr>),
    /// `*`
    Mul(Box<Expr>, Box<Expr>),
    /// `/`
    Div(Box<Expr>, Box<Expr>),
    /// `%`
    Module(Box<Expr>),
    /// `&`
    BitAnd(Box<Expr>, Box<Expr>),
    /// `|`
    BitOr(Box<Expr>, Box<Expr>),
    /// `==`
    Equal(Box<Expr>, Box<Expr>),
}

impl BuiltInOperator {
    // pub fn parse_built_in_operator(str:&str)->IResult<&str,BuiltInOperator,VerboseError<&str>>{

    // }
}

#[derive(Debug, PartialEq)]
pub enum BuiltInType {
    Num(Number),

    String(String),

    Boolean(bool),
}
#[derive(Debug, PartialEq)]
pub enum Number {
    Integer(i64),
    Float(f64),
}

impl From<Number> for BuiltInType {
    fn from(value: Number) -> Self {
        return BuiltInType::Num(value);
    }
}

impl<'a> Number {
    fn parse_hex(str: &'a str) -> IResult<&'a str, Number, VerboseError<&str>> {
        map_res(
            preceded(
                alt((tag("0x"), tag("0X"))),
                recognize(many1(terminated(
                    one_of("0123456789abcdefABCDEF"),
                    many0(character::complete::char('_')),
                ))),
            ),
            |out: &str| i64::from_str_radix(&str::replace(&out, "_", ""), 16),
        )
        .parse(str)
        .map(|result| (result.0, Number::Integer(result.1)))
    }

    fn parse_oct(str: &'a str) -> IResult<&'a str, Number, VerboseError<&str>> {
        map_res(
            preceded(
                alt((tag("0o"), tag("0O"))),
                recognize(many1(terminated(
                    one_of("01234567"),
                    many0(character::complete::char('_')),
                ))),
            ),
            |out: &str| i64::from_str_radix(&str::replace(&out, "_", ""), 8),
        )
        .parse(str)
        .map(|result| (result.0, Number::Integer(result.1)))
    }

    fn parse_binary(str: &'a str) -> IResult<&'a str, Number, VerboseError<&str>> {
        map_res(
            preceded(
                alt((tag("0b"), tag("0B"))),
                recognize(many1(terminated(
                    one_of("01"),
                    many0(character::complete::char('_')),
                ))),
            ),
            |out: &str| i64::from_str_radix(&str::replace(&out, "_", ""), 2),
        )
        .parse(str)
        .map(|result| (result.0, Number::Integer(result.1)))
    }

    fn parse_dec(str: &'a str) -> IResult<&'a str, Number, VerboseError<&str>> {
        map_res(
            recognize(many1(terminated(
                one_of("0123456789"),
                many0(character::complete::char('_')),
            ))),
            |out: &str| i64::from_str_radix(&str::replace(&out, "_", ""), 10),
        )
        .parse(str)
        .map(|result| (result.0, Number::Integer(result.1)))
    }

    /// Parse the number with leading 0x/0X
    pub fn parse_integer(str: &'a str) -> IResult<&'a str, Number, VerboseError<&str>> {
        alt((
            Self::parse_hex,
            Self::parse_oct,
            Self::parse_binary,
            Self::parse_dec,
        ))
        .parse(str)
    }

    pub fn parse_double(str: &'a str) -> IResult<&'a str, Number, VerboseError<&str>> {
        double(str).map(|result| (result.0, Number::Float(result.1)))
    }
}

impl<'a> BuiltInType {
    #[allow(dead_code)]
    fn parse_num(str: &'a str) -> IResult<&'a str, BuiltInType, VerboseError<&str>> {
        alt((Number::parse_integer, Number::parse_double))
            .parse(str)
            .map(|result| (result.0, result.1.into()))
    }
    #[allow(dead_code)]
    fn parse_boolean(str: &'a str) -> IResult<&'a str, BuiltInType, VerboseError<&str>> {
        alt((
            map(tag("true"), |_| BuiltInType::Boolean(true)),
            map(tag("false"), |_| BuiltInType::Boolean(false)),
        ))
        .parse(str)
    }
    #[allow(dead_code)]
    fn parse_string(str: &'a str) -> IResult<&'a str, BuiltInType, VerboseError<&str>> {
        StringParser::parse_string::<VerboseError<&str>>(str)
            .map(|result| (result.0, BuiltInType::String(result.1)))
    }

    pub fn parse_built_in_type(str: &'a str) -> IResult<&'a str, BuiltInType, VerboseError<&str>> {
        alt((
            map(tag("true"), |_| BuiltInType::Boolean(true)),
            map(tag("false"), |_| BuiltInType::Boolean(false)),
        ))
        .parse(str)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_built_in_type() {
        let a = "true";
        assert_eq!(
            BuiltInType::parse_built_in_type(a).unwrap().1,
            BuiltInType::Boolean(true)
        )
    }

    #[test]
    fn test_parse_number() {
        let a = "0x1234";
        assert_eq!(
            BuiltInType::parse_num(a).unwrap().1,
            BuiltInType::Num(Number::Integer(4660))
        );
        let b = "0b1111";
        assert_eq!(
            BuiltInType::parse_num(b).unwrap().1,
            BuiltInType::Num(Number::Integer(15))
        );
        let c = "0.123";
        println!("{:?}", BuiltInType::parse_num(c))
    }
}
