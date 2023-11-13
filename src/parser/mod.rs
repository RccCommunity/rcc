use chumsky::span::SimpleSpan;

pub mod expr;
pub mod string_parser;
type Span = SimpleSpan<usize>;
type Spanned<T> = (T, Span);

// 通过引入 nom 或者类似的 parser 框架，实际上是把 lexer和syntax analysis的工作一起做了
pub struct RccParser;
