use nom::{
    branch::alt,
    bytes::complete::{tag, take_till},
    character::complete::{multispace1, one_of},
    combinator::{map, map_res},
    error::VerboseError,
    IResult, Parser,
};
//未附加属性的表达式
#[derive(Debug, PartialEq, Eq)]
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
#[derive(Debug, PartialEq, Eq)]
pub enum CodeBlock {
    Code(Vec<Box<Expr>>),

    ReturnCode(Box<Expr>),
}
#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
pub enum BuiltInType {
    Num(Digit),

    Char(String),

    Boolean(bool),
}
#[derive(Debug, PartialEq, Eq)]
pub enum Digit {
    Hex(isize),
    Dec(isize),
    Octal(isize),
    Binary(isize),
}

impl BuiltInType {
    ///
    pub fn parse_built_in_type(str: &str) -> IResult<&str, BuiltInType, VerboseError<&str>> {
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
}
