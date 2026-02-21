use std::fs;
use std::path::Path;

use tfel::evaluator::Evaluator;
use tfel::lexer::{LexError, Span, tokenize};
use tfel::parser::{ParseError, Parser};
use tfel::preprocessor::preprocess_source;

fn main() {
    if let Err(err) = run() {
        eprintln!("{err}");
        std::process::exit(1);
    }
}

fn run() -> Result<(), String> {
    let mut args = std::env::args().skip(1);
    let Some(path) = args.next() else {
        return Err("usage: cargo run -- <path/to/file.tfel>".to_string());
    };

    let source =
        fs::read_to_string(&path).map_err(|err| format!("failed to read '{}': {}", path, err))?;

    if Path::new(&path).extension().and_then(|ext| ext.to_str()) != Some("tfel") {
        eprintln!("warning: '{}' does not use the .tfel extension", path);
    }

    // Normal languages parse source mostly as written.
    // TFEL decodes mirrored lines and flips file order before parsing.
    let preprocessed = preprocess_source(&source);

    let tokens =
        tokenize(&preprocessed).map_err(|errors| format_lex_errors(&preprocessed, &errors))?;

    let program = Parser::new(tokens)
        .parse_program()
        .map_err(|errors| format_parse_errors(&preprocessed, &errors))?;

    // If this were Python/JS/etc., the file would already look sane here.
    // Instead we now evaluate the AST that survived the mirror world.
    let base_dir = Path::new(&path)
        .parent()
        .map(Path::to_path_buf)
        .unwrap_or_else(|| ".".into());
    let mut evaluator = Evaluator::with_base_dir(base_dir);
    let value = evaluator
        .eval_program(&program)
        .map_err(|err| err.to_string())?;

    println!("{}", value);
    Ok(())
}

fn format_lex_errors(source: &str, errors: &[LexError]) -> String {
    format_errors(
        source,
        errors
            .iter()
            .map(|err| (&err.message, err.span))
            .collect::<Vec<_>>(),
    )
}

fn format_parse_errors(source: &str, errors: &[ParseError]) -> String {
    format_errors(
        source,
        errors
            .iter()
            .map(|err| (&err.message, err.span))
            .collect::<Vec<_>>(),
    )
}

fn format_errors(source: &str, entries: Vec<(&String, Span)>) -> String {
    entries
        .into_iter()
        .map(|(message, span)| format_error(source, span, message))
        .collect::<Vec<_>>()
        .join("\n\n")
}

fn format_error(source: &str, span: Span, message: &str) -> String {
    let (line_no, column_no, line_text) = line_context(source, span.start);
    let caret_padding = " ".repeat(column_no.saturating_sub(1));

    format!(
        "{} at line {}, column {} (byte {}..{})\n{}\n{}^",
        message, line_no, column_no, span.start, span.end, line_text, caret_padding
    )
}

fn line_context(source: &str, byte_offset: usize) -> (usize, usize, String) {
    let bounded = clamp_to_char_boundary(source, byte_offset.min(source.len()));
    let mut line_no = 1usize;
    let mut line_start = 0usize;

    for segment in source.split_inclusive('\n') {
        let line_end = line_start + segment.len();
        if bounded < line_end {
            let text = segment.trim_end_matches('\n').to_string();
            let column = source[line_start..bounded].chars().count() + 1;
            return (line_no, column, text);
        }
        line_no += 1;
        line_start = line_end;
    }

    if source.is_empty() {
        return (1, 1, String::new());
    }

    let line = source.lines().last().unwrap_or_default().to_string();
    let column = line.chars().count() + 1;
    (line_no, column, line)
}

fn clamp_to_char_boundary(source: &str, mut idx: usize) -> usize {
    while idx > 0 && !source.is_char_boundary(idx) {
        idx -= 1;
    }
    idx
}
