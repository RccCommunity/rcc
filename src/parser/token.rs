use std::{fmt, ops::Neg};

use chumsky::{
    extra,
    prelude::Rich,
    primitive::{any, choice, end, just, none_of, one_of},
    recovery::skip_then_retry_until,
    span::SimpleSpan,
    text, IterParser, Parser,
};

pub type Span = SimpleSpan<usize>;

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'src> {
    Null,
    Bool(bool),
    Num(Number),
    Str(&'src str),
    Op(&'src str),
    Ctrl(char),
    Ident(&'src str),
    Fn,
    Let,
    Print,
    If,
    Else,
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
impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Number::Integer(num) => write!(f, "{}", num),
            Number::Float(num) => write!(f, "{}", num),
        }
    }
}

impl<'src> fmt::Display for Token<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Null => write!(f, "null"),
            Token::Bool(x) => write!(f, "{}", x),
            Token::Num(n) => write!(f, "{}", n),
            Token::Str(s) => write!(f, "{}", s),
            Token::Op(s) => write!(f, "{}", s),
            Token::Ctrl(c) => write!(f, "{}", c),
            Token::Ident(s) => write!(f, "{}", s),
            Token::Fn => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::Print => write!(f, "print"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
        }
    }
}

pub fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<(Token<'src>, Span)>, extra::Err<Rich<'src, char, Span>>> {
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
            .map(|res| str::replace(res, "_", "").parse::<isize>()),
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
        res.parse::<f64>().unwrap_or_else(|_| {
            panic!(
                "Panic! Failed to parse the ans:{:?} while parsing  float",
                res
            )
        })
    });

    let parse_float = choice((
        just('-').ignore_then(parse_pos_float.map(|res| Number::Float(-res))), // Handle the leading minus sign
        parse_pos_float.map(Number::Float),
    ));

    let parse_integer = choice((
        just('-').ignore_then(parse_pos_integer.map(|res| Number::Integer(-res))),
        parse_pos_integer.map(Number::Integer),
    ));
    // A parser for numbers
    let parse_number = choice((parse_float.map(Token::Num), parse_integer.map(Token::Num)));
    // A parser for strings
    let parse_str = just('"')
        .ignore_then(none_of('"').repeated())
        .then_ignore(just('"'))
        .to_slice()
        .map(Token::Str);

    // A parser for operators
    let parse_op = one_of("+*-/!=")
        .repeated()
        .at_least(1)
        .to_slice()
        .map(Token::Op);

    // A parser for control characters (delimiters, semicolons, etc.)
    let parse_ctrl = one_of("()[]{};,").map(Token::Ctrl);

    // A parser for identifiers and keywords
    let parse_ident = text::ascii::ident().map(|ident: &str| match ident {
        "fn" => Token::Fn,
        "let" => Token::Let,
        "print" => Token::Print,
        "if" => Token::If,
        "else" => Token::Else,
        "true" => Token::Bool(true),
        "false" => Token::Bool(false),
        "null" => Token::Null,
        _ => Token::Ident(ident),
    });

    // A single token can be one of the above
    let token = parse_number
        .or(parse_str)
        .or(parse_op)
        .or(parse_ctrl)
        .or(parse_ident);

    let comment = just("//")
        .then(any().and_is(just('\n').not()).repeated())
        .padded();

    token
        .map_with(|tok, e| (tok, e.span()))
        .padded_by(comment.repeated())
        .padded()
        // If we encounter an error, skip and attempt to lex the next character as a token instead
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        assert_eq!(
            lexer().parse("1+2+3").output(),
            Some(&vec![
                (Token::Num(Number::Integer(1)), SimpleSpan::new(0, 1)),
                (Token::Op("+"), SimpleSpan::new(1, 2)),
                (Token::Num(Number::Integer(2)), SimpleSpan::new(2, 3)),
                (Token::Op("+"), SimpleSpan::new(3, 4)),
                (Token::Num(Number::Integer(3)), SimpleSpan::new(4, 5))
            ])
        );
        println!("{:?}", lexer().parse("fns"))
    }
}
