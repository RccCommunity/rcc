use std::{collections::HashMap, fs};

use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::{
    prelude::{Input, Rich},
    Parser,
};

use super::{
    expr::{funcs_parser, Func},
    token::{lexer, Token},
};

// 通过引入 nom 或者类似的 parser 框架，实际上是把 lexer和syntax analysis的工作一起做了
pub struct RccParser;

impl RccParser {
    pub fn parse(filename: &'static str) -> Result<&HashMap<&str, Func>, ()> {
        let src = fs::read_to_string(filename).expect("Failed to read file");
        let (tokens, mut errs) = lexer().parse(src.as_str()).into_output_errors();

        let parse_errs: Vec<Rich<'_, Token<'_>>> = if let Some(tokens) = &tokens {
            let (ast, parse_errs) = funcs_parser()
                .map_with(|ast, e| (ast, e.span()))
                .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
                .into_output_errors();
            if parse_errs.is_empty() {
                // !!! Havn't handle unrecoverable erros, so here might so errors here
                return Err(());
            }
            if let Some((funcs, file_span)) = ast.filter(|_| errs.len() + parse_errs.len() == 0) {
                if let Some(main) = funcs.get("main") {
                    if main.args().len() != 0 {
                        errs.push(Rich::custom(
                            main.span(),
                            format!("The main function cannot have arguments"),
                        ))
                    }
                } else {
                    errs.push(Rich::custom(
                        file_span,
                        format!("Programs need a main function but none was found"),
                    ));
                }
            }

            parse_errs
        } else {
            Vec::new()
        };

        errs.into_iter()
            .map(|e| e.map_token(|c| c.to_string()))
            .chain(
                parse_errs
                    .into_iter()
                    .map(|e| e.map_token(|tok| tok.to_string())),
            )
            .for_each(|e| {
                Report::build(ReportKind::Error, filename.clone(), e.span().start)
                    .with_message(e.to_string())
                    .with_label(
                        Label::new((filename.clone(), e.span().into_range()))
                            .with_message(e.reason().to_string())
                            .with_color(Color::Red),
                    )
                    .with_labels(e.contexts().map(|(label, span)| {
                        Label::new((filename.clone(), span.into_range()))
                            .with_message(format!("while parsing this {}", label))
                            .with_color(Color::Yellow)
                    }))
                    .finish()
                    .print(sources([(filename.clone(), src.clone())]))
                    .unwrap()
            });
        Err(())
    }
}
