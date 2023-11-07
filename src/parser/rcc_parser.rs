use nom::error::{VerboseError, VerboseErrorKind};

use super::expr::Expr;

// 通过引入 nom 或者类似的 parser 框架，实际上是把 lexer和syntax analysis的工作一起做了
pub struct RccParser;

impl RccParser {
    pub fn parse(_input: &str) -> Result<Vec<Expr>, VerboseError<&str>> {
        Err(VerboseError {
            errors: vec![(
                "failed to parse input",
                VerboseErrorKind::Context("failed to parse input"),
            )],
        })
    }
}
