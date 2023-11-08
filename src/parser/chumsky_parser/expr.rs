use chumsky::{extra, prelude::*, Parser};
use std::ops::Neg;

//未附加属性的表达式
#[derive(Debug, PartialEq)]
pub enum Expr {
    Invalid,

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
    Module(Box<Expr>, Box<Expr>),
    /// `&`
    BitAnd(Box<Expr>, Box<Expr>),
    /// `|`
    BitOr(Box<Expr>, Box<Expr>),
    /// `==`
    Equal(Box<Expr>, Box<Expr>),
    /// `!=`
    NotEqual(Box<Expr>, Box<Expr>),
}

#[derive(Debug, PartialEq)]
pub enum BuiltInType {
    Num(Number),

    String(String),

    Boolean(bool),
}
#[derive(Debug, PartialEq)]
pub enum Number {
    Integer(isize),
    Float(f64),
}

impl Neg for Number {
    type Output = Number;

    fn neg(self) -> Self::Output {
        match self {
            Number::Integer(data) => Number::Integer(-data),
            Number::Float(data) => Number::Float(-data),
        }
    }
}

impl From<Number> for BuiltInType {
    fn from(value: Number) -> Self {
        BuiltInType::Num(value)
    }
}

fn parser<'a>() -> impl Parser<'a, &'a str, Expr, extra::Err<Rich<'a, char>>> {
    recursive(|value| {
        let parse_pos_num = choice((
            just("0x").or(just("0X")).then(text::digits(16)).to_slice(),
            just("0o").or(just("0O")).then(text::digits(8)).to_slice(),
            just("0b").or(just("0B")).then(text::digits(2)).to_slice(),
            text::digits(10).to_slice(),
        ));

        let parse_pos_float = choice((
            text::digits(10)
                .padded()
                .separated_by(just('.'))
                .exactly(2)
                .collect::<Vec<_>>(),
            text::digits(10)
                .padded()
                .separated_by(just("E-"))
                .exactly(2)
                .collect::<Vec<_>>(),
            text::digits(10)
                .padded()
                .separated_by(just("K-"))
                .exactly(2)
                .collect::<Vec<_>>(),
        ));

        let parse_float = choice((
            just('-')
                .then(parse_pos_float)
                .to_slice()
                .map(|res: &str| Number::Float(-res.parse::<f64>().unwrap())),
            parse_pos_float
                .to_slice()
                .map(|res: &str| Number::Float(res.parse::<f64>().unwrap())),
        ));

        let parse_number = choice((
            just('-')
                .then(parse_pos_num)
                .to_slice()
                .map(|res: &str| Number::Integer(-res.parse::<isize>().unwrap())),
            parse_pos_num.map(|res: &str| Number::Integer(res.parse::<isize>().unwrap())),
        ));

        choice((
            parse_float.map(|res| Expr::Literal(BuiltInType::Num(res))),
            parse_number.map(|res| Expr::Literal(BuiltInType::Num(res))),
        ))
        .recover_with(via_parser(nested_delimiters(
            '{',
            '}',
            [('[', ']')],
            |_| Expr::Invalid,
        )))
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser() {
        assert_eq!(
            parser().parse("123E-23").output(),
            Some(&Expr::Literal(BuiltInType::Num(Number::Float(1.23e-21))))
        );
        assert_eq!(
            parser().parse("1.23").output(),
            Some(&Expr::Literal(BuiltInType::Num(Number::Float(1.23))))
        );
        assert_eq!(
            parser().parse("123").output(),
            Some(&Expr::Literal(BuiltInType::Num(Number::Integer(123))))
        );
    }
}
