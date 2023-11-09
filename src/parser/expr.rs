use chumsky::{extra, prelude::*, Parser};
use std::collections::HashMap;

use super::token::{Number, Token};

//未附加属性的表达式
#[derive(Clone, Debug, PartialEq)]
pub enum Expr<'src> {
    Error,

    Literal(BuiltInType<'src>),

    List(Vec<Spanned<Self>>),
    //局部变量
    Local(&'src str),
    //暂时猜的是在Assign的过程中如果检查到符号表里没有就插入
    // 声明语句来实现声明+赋值
    Let(&'src str, Box<Spanned<Self>>, Box<Spanned<Self>>),

    Then(Box<Spanned<Self>>, Box<Spanned<Self>>),
    // //属性后置 形如 let a : int;
    // VarDeclaration(String, String),
    // Fuction Call
    Call(Box<Spanned<Self>>, Spanned<Vec<Spanned<Self>>>),

    Block(CodeBlock<'src>),

    If(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),

    WhileBlock(Box<Spanned<Self>>, CodeBlock<'src>),

    Operator(BuiltInOperator<'src>),
}
#[derive(Clone, Debug, PartialEq)]
pub enum CodeBlock<'src> {
    Code(Spanned<Vec<Box<Spanned<Expr<'src>>>>>),

    ReturnCode(Box<Spanned<Expr<'src>>>),
}

/// Temporary to store a middle answer while parsing expression, might be optimized out in one day
#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOps {
    Add,
    Sub,
    Mul,
    Div,
    Module,
    BitAnd,
    BitOr,
    Equal,
    NotEqual,
}

/// Temporary to store a middle answer while parsing expression, might be optimized out in one day
#[derive(Clone, Debug, PartialEq)]
pub struct TempBuiltInOperator<'src>(
    pub  (
        Box<Spanned<Expr<'src>>>,
        BinaryOps,
        Box<Spanned<Expr<'src>>>,
    ),
);
impl<'src> From<TempBuiltInOperator<'src>> for Expr<'src> {
    fn from(val: TempBuiltInOperator<'src>) -> Self {
        match val.0 .1 {
            BinaryOps::Add => Expr::Operator(BuiltInOperator::Add(val.0 .0, val.0 .2)),
            BinaryOps::Sub => Expr::Operator(BuiltInOperator::Sub(val.0 .0, val.0 .2)),
            BinaryOps::Mul => Expr::Operator(BuiltInOperator::Mul(val.0 .0, val.0 .2)),
            BinaryOps::Div => Expr::Operator(BuiltInOperator::Div(val.0 .0, val.0 .2)),
            BinaryOps::Module => Expr::Operator(BuiltInOperator::Module(val.0 .0, val.0 .2)),
            BinaryOps::BitAnd => Expr::Operator(BuiltInOperator::BitAnd(val.0 .0, val.0 .2)),
            BinaryOps::BitOr => Expr::Operator(BuiltInOperator::BitOr(val.0 .0, val.0 .2)),
            BinaryOps::Equal => Expr::Operator(BuiltInOperator::Equal(val.0 .0, val.0 .2)),
            BinaryOps::NotEqual => Expr::Operator(BuiltInOperator::NotEqual(val.0 .0, val.0 .2)),
        }
    }
}
#[derive(Clone, Debug, PartialEq)]
// 连接运算符
pub enum BuiltInOperator<'src> {
    /// `+`
    Add(Box<Spanned<Expr<'src>>>, Box<Spanned<Expr<'src>>>),
    /// `-`
    Sub(Box<Spanned<Expr<'src>>>, Box<Spanned<Expr<'src>>>),
    /// `*`
    Mul(Box<Spanned<Expr<'src>>>, Box<Spanned<Expr<'src>>>),
    /// `/`
    Div(Box<Spanned<Expr<'src>>>, Box<Spanned<Expr<'src>>>),
    /// `%`
    Module(Box<Spanned<Expr<'src>>>, Box<Spanned<Expr<'src>>>),
    /// `&`
    BitAnd(Box<Spanned<Expr<'src>>>, Box<Spanned<Expr<'src>>>),
    /// `|`
    BitOr(Box<Spanned<Expr<'src>>>, Box<Spanned<Expr<'src>>>),
    /// `==`
    Equal(Box<Spanned<Expr<'src>>>, Box<Spanned<Expr<'src>>>),
    /// `!=`
    NotEqual(Box<Spanned<Expr<'src>>>, Box<Spanned<Expr<'src>>>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum BuiltInType<'src> {
    Null,

    Num(Number),

    String(&'src str),

    Boolean(bool),
}

impl<'src> From<Number> for BuiltInType<'src> {
    fn from(value: Number) -> Self {
        BuiltInType::Num(value)
    }
}

//TODO Use span to generate more useful runtime error messages
pub type Span = SimpleSpan<usize>;
pub type Spanned<T> = (T, Span);

// The type of the input that our parser operates on. The input is the `&[(Token, Span)]` token buffer generated by the
// lexer, wrapped in a `SpannedInput` which 'splits' it apart into its constituent parts, tokens and spans, for chumsky
// to understand.
type ParserInput<'src> =
    chumsky::input::SpannedInput<Token<'src>, Span, &'src [(Token<'src>, Span)]>;

// This looks complex, but don't be scared!
//
// There are two lifetimes here:
//     - 'src: the lifetime of the underlying source code (the string we read from disk)
//     - 'tokens: the lifetime of the token buffer emitted by the lexer
// Our source code lives longer than the token buffer, hence `'src: 'tokens`
//
// From this function, we return a parser that parses an input of type `ParserInput` (see above for an explanation of
// that) and produces a `Spanned<Expr>` (an expression with a span attached to it, so we can point to the right thing
// for runtime errors).
//
// We also specify an error type used by the parser. In this case, it's `Rich`, one of chumsky's default error types.
pub fn expr_parser<'src>() -> impl Parser<
    'src,
    ParserInput<'src>,
    Spanned<Expr<'src>>,
    extra::Err<Rich<'src, Token<'src>, Span>>,
> + Clone {
    recursive(|expr| {
        let inline_expr = recursive(|inline_expr| {
            let parse_built_in_type = select! {
                Token::Null=>Expr::Literal(BuiltInType::Null),
                Token::Num(x)=> Expr::Literal(BuiltInType::Num(x)),
                Token::Str(x)=>Expr::Literal(BuiltInType::String(x)),
                Token::Bool(x)=>Expr::Literal(BuiltInType::Boolean(x))
            }
            .labelled("value");

            let parse_ident = select! { Token::Ident(ident) => ident }.labelled("identifier");
            // A list of expressions
            let parse_items = expr
                .clone()
                .separated_by(just(Token::Ctrl(',')))
                .allow_trailing()
                .collect::<Vec<_>>();

            // A let expression
            let parse_let = just(Token::Let)
                .ignore_then(parse_ident)
                .then_ignore(just(Token::Op("=")))
                .then(inline_expr)
                .then_ignore(just(Token::Ctrl(';')))
                .then(expr.clone())
                .map(|((name, val), body)| Expr::Let(name, Box::new(val), Box::new(body)));
            let parse_list = parse_items
                .clone()
                .map(Expr::List)
                .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')));

            // 'Atoms' are expressions that contain no ambiguity
            let atom = parse_built_in_type
                .or(parse_ident.map(Expr::Local))
                .or(parse_let)
                .or(parse_list)
                // In Nano Rust, `print` is just a keyword, just like Python 2, for simplicity
                .map_with(|expr, e| (expr, e.span()))
                // Atoms can also just be normal expressions, but surrounded with parentheses
                .or(expr
                    .clone()
                    .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))))
                // Attempt to recover anything that looks like a parenthesised expression but contains errors
                .recover_with(via_parser(nested_delimiters(
                    Token::Ctrl('('),
                    Token::Ctrl(')'),
                    [
                        (Token::Ctrl('['), Token::Ctrl(']')),
                        (Token::Ctrl('{'), Token::Ctrl('}')),
                    ],
                    |span| (Expr::Error, span),
                )))
                // Attempt to recover anything that looks like a list but contains errors
                .recover_with(via_parser(nested_delimiters(
                    Token::Ctrl('['),
                    Token::Ctrl(']'),
                    [
                        (Token::Ctrl('('), Token::Ctrl(')')),
                        (Token::Ctrl('{'), Token::Ctrl('}')),
                    ],
                    |span| (Expr::Error, span),
                )))
                .boxed();

            // Function calls have very high precedence so we prioritise them
            let call = atom.foldl_with(
                parse_items
                    .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
                    .map_with(|args, e| (args, e.span()))
                    .repeated(),
                |f, args, e| (Expr::Call(Box::new(f), args), e.span()),
            );

            let op = just(Token::Op("*"))
                .to(BinaryOps::Mul)
                .or(just(Token::Op("/")).to(BinaryOps::Div));
            let product = call
                .clone()
                .foldl_with(op.then(call).repeated(), |a, (op, b), e| {
                    (
                        TempBuiltInOperator((Box::new(a), op, Box::new(b))).into(),
                        e.span(),
                    )
                });
            // Sum ops (add and subtract) have equal precedence
            let op = just(Token::Op("+"))
                .to(BinaryOps::Add)
                .or(just(Token::Op("-")).to(BinaryOps::Sub));
            let sum = product
                .clone()
                .foldl_with(op.then(product).repeated(), |a, (op, b), e| {
                    (
                        TempBuiltInOperator((Box::new(a), op, Box::new(b))).into(),
                        e.span(),
                    )
                });

            // Comparison ops (equal, not-equal) have equal precedence
            let op = just(Token::Op("=="))
                .to(BinaryOps::Equal)
                .or(just(Token::Op("!=")).to(BinaryOps::NotEqual));
            let compare = sum
                .clone()
                .foldl_with(op.then(sum).repeated(), |a, (op, b), e| {
                    (
                        TempBuiltInOperator((Box::new(a), op, Box::new(b))).into(),
                        e.span(),
                    )
                });

            compare.labelled("expression").as_context()
        });

        // Blocks are expressions but delimited with braces
        let block = expr
            .clone()
            .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
            // Attempt to recover anything that looks like a block but contains errors
            .recover_with(via_parser(nested_delimiters(
                Token::Ctrl('{'),
                Token::Ctrl('}'),
                [
                    (Token::Ctrl('('), Token::Ctrl(')')),
                    (Token::Ctrl('['), Token::Ctrl(']')),
                ],
                |span| (Expr::Error, span),
            )));

        let if_ = recursive(|if_| {
            just(Token::If)
                .ignore_then(expr.clone())
                .then(block.clone())
                .then(
                    just(Token::Else)
                        .ignore_then(block.clone().or(if_))
                        .or_not(),
                )
                .map_with(|((cond, a), b), e| {
                    (
                        Expr::If(
                            Box::new(cond),
                            Box::new(a),
                            // If an `if` expression has no trailing `else` block, we magic up one that just produces null
                            Box::new(
                                b.unwrap_or_else(|| (Expr::Literal(BuiltInType::Null), e.span())),
                            ),
                        ),
                        e.span(),
                    )
                })
        });

        let block_expr = block.or(if_);

        let block_chain = block_expr
            .clone()
            .foldl_with(block_expr.clone().repeated(), |a, b, e| {
                (Expr::Then(Box::new(a), Box::new(b)), e.span())
            });

        let block_recovery = nested_delimiters(
            Token::Ctrl('{'),
            Token::Ctrl('}'),
            [
                (Token::Ctrl('('), Token::Ctrl(')')),
                (Token::Ctrl('['), Token::Ctrl(']')),
            ],
            |span| (Expr::Error, span),
        );
        block_chain
            .labelled("block")
            // Expressions, chained by semicolons, are statements
            .or(inline_expr.clone())
            .recover_with(skip_then_retry_until(
                block_recovery.ignored().or(any().ignored()),
                one_of([
                    Token::Ctrl(';'),
                    Token::Ctrl('}'),
                    Token::Ctrl(')'),
                    Token::Ctrl(']'),
                ])
                .ignored(),
            ))
            .foldl_with(
                just(Token::Ctrl(';')).ignore_then(expr.or_not()).repeated(),
                |a, b, e| {
                    let span: Span = e.span();
                    (
                        Expr::Then(
                            Box::new(a),
                            // If there is no b expression then its span is the end of the statement/block.
                            Box::new(b.unwrap_or_else(|| {
                                (Expr::Literal(BuiltInType::Null), span.to_end())
                            })),
                        ),
                        span,
                    )
                },
            )
    })

    // let parse_built_in_ops = recursive(|ops| {
    //     choice((ops
    //         .clone()
    //         .separated_by(just('+'))
    //         .exactly(2)
    //         .collect::<Vec<Expr>>()
    //         .map(|res| {
    //             Expr::Operator(BuiltInOperator::Add(
    //                 Box::new(
    //                     res.get(0)
    //                         .clone()
    //                         .expect("Panic! Failed to retrieve lhs operands while parsing '+' ops")
    //                         .clone(),
    //                 ),
    //                 Box::new(
    //                     res.get(1)
    //                         .clone()
    //                         .expect("Panic! Failed to retrieve rhs operands while parsing '+' ops")
    //                         .clone(),
    //                 ),
    //             ))
    //         }),))
    // });

    // .recover_with(via_parser(nested_delimiters(
    //     '{',
    //     '}',
    //     [('[', ']')],
    //     |_| Expr::Invalid,
    // )))
}
// A function node in the AST.
#[derive(Debug)]
pub struct Func<'src> {
    args: Vec<&'src str>,
    span: Span,
    body: Spanned<Expr<'src>>,
}

impl<'src> Func<'src> {
    pub fn args(&self) -> &[&str] {
        self.args.as_ref()
    }

    pub fn span(&self) -> SimpleSpan<usize, ()> {
        self.span
    }

    pub fn body(&self) -> &Spanned<Expr<'src>> {
        &self.body
    }
}
pub fn funcs_parser<'src>() -> impl Parser<
    'src,
    ParserInput<'src>,
    HashMap<&'src str, Func<'src>>,
    extra::Err<Rich<'src, Token<'src>, Span>>,
> + Clone {
    let ident = select! { Token::Ident(ident) => ident };

    // Argument lists are just identifiers separated by commas, surrounded by parentheses
    let args = ident
        .separated_by(just(Token::Ctrl(',')))
        .allow_trailing()
        .collect()
        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
        .labelled("function args");

    let func = just(Token::Fn)
        .ignore_then(
            ident
                .map_with(|name, e| (name, e.span()))
                .labelled("function name"),
        )
        .then(args)
        .map_with(|start, e| (start, e.span()))
        .then(
            expr_parser()
                .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
                // Attempt to recover anything that looks like a function body but contains errors
                .recover_with(via_parser(nested_delimiters(
                    Token::Ctrl('{'),
                    Token::Ctrl('}'),
                    [
                        (Token::Ctrl('('), Token::Ctrl(')')),
                        (Token::Ctrl('['), Token::Ctrl(']')),
                    ],
                    |span| (Expr::Error, span),
                ))),
        )
        .map(|(((name, args), span), body)| (name, Func { args, span, body }))
        .labelled("function");

    func.repeated()
        .collect::<Vec<_>>()
        .validate(|fs, _, emitter| {
            let mut funcs = HashMap::new();
            for ((name, name_span), f) in fs {
                if funcs.insert(name, f).is_some() {
                    emitter.emit(Rich::custom(
                        name_span,
                        format!("Function '{}' already exists", name),
                    ));
                }
            }
            funcs
        })
}

// #[cfg(test)]
// mod tests {
//     use std::fs;

//     use ariadne::{sources, Color, Label, Report, ReportKind};

//     use crate::parser::chumsky_parser::token::lexer;

//     use super::*;

//     #[test]
//     fn test_parser() {
//         let filename = "/home/chi/coding/compiler/rcc/src/parser/chumsky_parser/test.rcc";

//         let src = fs::read_to_string(&filename).expect("Failed to read file");
//         let (tokens, mut errs) = lexer().parse(src.as_str()).into_output_errors();

//         let parse_errs: Vec<Rich<'_, Token<'_>>> = if let Some(tokens) = &tokens {
//             let (ast, parse_errs) = funcs_parser()
//                 .map_with(|ast, e| (ast, e.span()))
//                 .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
//                 .into_output_errors();

//             if let Some((funcs, file_span)) = ast.filter(|_| errs.len() + parse_errs.len() == 0) {
//                 if let Some(main) = funcs.get("main") {
//                     if main.args.len() != 0 {
//                         errs.push(Rich::custom(
//                             main.span,
//                             format!("The main function cannot have arguments"),
//                         ))
//                     }
//                 } else {
//                     errs.push(Rich::custom(
//                         file_span,
//                         format!("Programs need a main function but none was found"),
//                     ));
//                 }
//             }

//             parse_errs
//         } else {
//             Vec::new()
//         };

//         errs.into_iter()
//             .map(|e| e.map_token(|c| c.to_string()))
//             .chain(
//                 parse_errs
//                     .into_iter()
//                     .map(|e| e.map_token(|tok| tok.to_string())),
//             )
//             .for_each(|e| {
//                 Report::build(ReportKind::Error, filename.clone(), e.span().start)
//                     .with_message(e.to_string())
//                     .with_label(
//                         Label::new((filename.clone(), e.span().into_range()))
//                             .with_message(e.reason().to_string())
//                             .with_color(Color::Red),
//                     )
//                     .with_labels(e.contexts().map(|(label, span)| {
//                         Label::new((filename.clone(), span.into_range()))
//                             .with_message(format!("while parsing this {}", label))
//                             .with_color(Color::Yellow)
//                     }))
//                     .finish()
//                     .print(sources([(filename.clone(), src.clone())]))
//                     .unwrap()
//             });
//     }
// }
