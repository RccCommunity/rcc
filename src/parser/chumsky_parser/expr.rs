use chumsky::{extra, prelude::*, Parser};
use std::ops::Neg;

//未附加属性的表达式
#[derive(Clone, Debug, PartialEq)]
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
#[derive(Clone, Debug, PartialEq)]
pub enum CodeBlock {
    Code(Vec<Box<Expr>>),

    ReturnCode(Box<Expr>),
}
#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
pub enum BuiltInType {
    Num(Number),

    String(String),

    Boolean(bool),
}
#[derive(Clone, Debug, PartialEq)]
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

//TODO Use span to generate more useful runtime error messages
pub type Span = SimpleSpan<usize>;
pub type Spanned<T> = (T, Span);

fn parser<'src>() -> impl Parser<'src, &'src str, Expr, extra::Err<Rich<'src, char>>> {
    recursive(|value| {
        let parse_pos_integer = choice((
            just("0x").or(just("0X")).ignore_then(
                text::digits(16)
                    .to_slice()
                    .map(|res| isize::from_str_radix(&str::replace(res, "_", ""), 16)),
            ),
            just("0o").or(just("0O")).ignore_then(
                text::digits(8)
                    .to_slice()
                    .map(|res| isize::from_str_radix(&str::replace(res, "_", ""), 8)),
            ),
            just("0b").or(just("0B")).ignore_then(
                text::digits(2)
                    .to_slice()
                    .map(|res| isize::from_str_radix(&str::replace(res, "_", ""), 2)),
            ),
            text::digits(10)
                .to_slice()
                .map(|res| isize::from_str_radix(&str::replace(res, "_", ""), 10)),
        ))
        .map(|res| res.expect("Failed to parse number"));

        let parse_pos_float = choice((
            text::digits(10)
                .separated_by(just('.'))
                .exactly(2)
                .collect::<Vec<_>>(),
            text::digits(10)
                .separated_by(just("E-"))
                .exactly(2)
                .collect::<Vec<_>>(),
            text::digits(10)
                .separated_by(just("K-"))
                .exactly(2)
                .collect::<Vec<_>>(),
        ))
        .to_slice()
        .map(|res: &str| {
            res.parse::<f64>().expect(&format!(
                "Panic! Failed to parse the ans:{:?} while parsing  float",
                res
            ))
        });

        let parse_float = choice((
            just('-').ignore_then(parse_pos_float.map(|res| Number::Float(-res))), // Handle the leading minus sign
            parse_pos_float.map(|res| Number::Float(res)),
        ));

        let parse_integer = choice((
            just('-').ignore_then(parse_pos_integer.map(|res| Number::Integer(-res))),
            parse_pos_integer.map(|res| Number::Integer(res)),
        ));

        let parse_boolean = choice((just("true").map(|_| true), just("false").map(|_| false)));

        let parse_string = just('"')
            .ignore_then(none_of('"').repeated())
            .then_ignore(just('"'))
            .to_slice();

        let parse_built_in_type = choice((
            parse_float.map(|res| BuiltInType::Num(res)),
            parse_integer.map(|res| BuiltInType::Num(res)),
            parse_boolean.map(|res| BuiltInType::Boolean(res)),
            parse_string.map(|res: &str| BuiltInType::String(res.to_string())),
        ));

        let parse_built_in_ops = choice((value
            .clone()
            .separated_by(just('+'))
            .exactly(2)
            .collect::<Vec<Expr>>()
            .map(|res| {
                BuiltInOperator::Add(
                    Box::new(
                        res.get(0)
                            .clone()
                            .expect("Panic! Failed to retrieve lhs operands while parsing '+' ops")
                            .clone(),
                    ),
                    Box::new(
                        res.get(1)
                            .clone()
                            .expect("Panic! Failed to retrieve rhs operands while parsing '+' ops")
                            .clone(),
                    ),
                )
            }),));

        choice((
            parse_built_in_ops.map(|res| Expr::Operator(res)),
            parse_built_in_type.map(|res| Expr::Literal(res)),
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
        println!("{:?}", parser().parse("123E-23"));
        assert_eq!(
            parser().parse("123E-23").output(),
            Some(&Expr::Literal(BuiltInType::Num(Number::Float(1.23e-21))))
        );
        assert_eq!(
            parser().parse("-123E-23").output(),
            Some(&Expr::Literal(BuiltInType::Num(Number::Float(-1.23e-21))))
        );
        assert_eq!(
            parser().parse("1.23").output(),
            Some(&Expr::Literal(BuiltInType::Num(Number::Float(1.23))))
        );
        assert_eq!(
            parser().parse("123").output(),
            Some(&Expr::Literal(BuiltInType::Num(Number::Integer(123))))
        );
        assert_eq!(
            parser().parse("-0x12DC7").output(),
            Some(&Expr::Literal(BuiltInType::Num(Number::Integer(-77255))))
        );
        assert_eq!(
            parser().parse("0x12DC7").output(),
            Some(&Expr::Literal(BuiltInType::Num(Number::Integer(77255))))
        );
        assert_eq!(
            parser().parse("0b11011").output(),
            Some(&Expr::Literal(BuiltInType::Num(Number::Integer(27))))
        );
        assert_eq!(
            parser().parse("0o547").output(),
            Some(&Expr::Literal(BuiltInType::Num(Number::Integer(359))))
        );

        println!("{:?}", parser().parse("1+2+3"));
    }
}
