pub mod expr;
type Span = SimpleSpan<usize>;
type Spanned<T> = (T, Span);
use chumsky::span::SimpleSpan;
// 通过引入 nom 或者类似的 parser 框架，实际上是把 lexer和syntax analysis的工作一起做了
pub struct RccParser;
