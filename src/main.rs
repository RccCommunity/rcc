pub mod codegen;
pub mod lexer;
pub mod parser;
extern crate ariadne;
extern crate chumsky;

use std::{env, fs};

use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::{
    prelude::{Input, Rich},
    Parser,
};
use codegen::code_generator::CodeGenerator;
use lexer::RccLexer;
use parser::RccParser;

fn main() {
    let filename = env::args().nth(1).expect("Expected file argument");

    let src = fs::read_to_string(&filename).expect("Failed to read file");

    let (tokens, mut errs) = RccLexer::lexer().parse(src.as_str()).into_output_errors();

    let (ast, parse_errs) = if let Some(tokens) = &tokens {
        let (ast, parse_errs) = RccParser::funcs_parser()
            .map_with(|ast, e| (ast, e.span()))
            .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
            .into_output_errors();
        if let Some((funcs, file_span)) = &ast {
            if let Some(main) = funcs.get("main") {
                if !main.args().is_empty() {
                    errs.push(Rich::custom(
                        main.span(),
                        "The main function cannot have arguments".to_string(),
                    ))
                }
            } else {
                errs.push(Rich::custom(
                    *file_span,
                    "Programs need a main function but none was found".to_string(),
                ));
            }
        }

        (ast, parse_errs)
    } else {
        (None, Vec::new())
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
    unsafe {
        CodeGenerator::codegen(
            ast.unwrap_or_else(|| panic!("Failed to generate an ast on file {:?}", filename)),
        )
    }
}
