use std::fs;
use std::path::{Path, PathBuf};

use tfel::evaluator::{Evaluator, EvaluatorOptions, RuntimePermissions};
use tfel::lexer::{LexError, LexOptions, Span, tokenize_with_options};
use tfel::parser::{ParseError, Parser, ParserOptions};
use tfel::preprocessor::{SourceMap, preprocess_source, preprocess_source_with_map};

enum Command {
    Run(RunConfig),
    Mirror { input: String, output: String },
}

struct RunConfig {
    path: String,
    logical: bool,
    strict_tfel: bool,
    allow_fs: bool,
    allow_net: bool,
    module_paths: Vec<String>,
}

fn main() {
    if let Err(err) = run() {
        eprintln!("{err}");
        std::process::exit(1);
    }
}

fn run() -> Result<(), String> {
    match parse_command(std::env::args().skip(1))? {
        Command::Run(config) => run_file(config),
        Command::Mirror { input, output } => mirror_file(&input, &output),
    }
}

fn parse_command(mut args: impl Iterator<Item = String>) -> Result<Command, String> {
    let Some(first) = args.next() else {
        return Err(usage());
    };

    if first == "mirror" {
        let input = args
            .next()
            .ok_or_else(|| "usage: tfel mirror <logical-input> <mirrored-output>".to_string())?;
        let output = args
            .next()
            .ok_or_else(|| "usage: tfel mirror <logical-input> <mirrored-output>".to_string())?;
        if args.next().is_some() {
            return Err(
                "mirror accepts exactly 2 arguments: <logical-input> <mirrored-output>".to_string(),
            );
        }
        return Ok(Command::Mirror { input, output });
    }

    let mut logical = false;
    let mut strict_tfel = false;
    let mut allow_fs = false;
    let mut allow_net = false;
    let mut module_paths = Vec::new();
    let mut path: Option<String> = None;
    let mut iter = std::iter::once(first).chain(args);

    while let Some(arg) = iter.next() {
        match arg.as_str() {
            "--logical" => logical = true,
            "--strict-tfel" => strict_tfel = true,
            "--allow-fs" => allow_fs = true,
            "--allow-net" => allow_net = true,
            "--module-path" => {
                let next = iter.next().ok_or_else(|| {
                    "missing value for '--module-path' (expected a directory path)".to_string()
                })?;
                module_paths.push(next);
            }
            "--help" | "-h" => return Err(usage()),
            _ if arg.starts_with("--module-path=") => {
                let value = arg.trim_start_matches("--module-path=");
                if value.is_empty() {
                    return Err(
                        "missing value for '--module-path' (expected a directory path)".to_string(),
                    );
                }
                module_paths.push(value.to_string());
            }
            _ if arg.starts_with('-') => {
                return Err(format!("unknown flag '{arg}'\n\n{}", usage()));
            }
            _ => {
                if path.is_some() {
                    return Err(format!(
                        "expected a single input path, got extra argument '{}'",
                        arg
                    ));
                }
                path = Some(arg);
            }
        }
    }

    let Some(path) = path else {
        return Err(usage());
    };

    Ok(Command::Run(RunConfig {
        path,
        logical,
        strict_tfel,
        allow_fs,
        allow_net,
        module_paths,
    }))
}

fn run_file(config: RunConfig) -> Result<(), String> {
    let source = fs::read_to_string(&config.path)
        .map_err(|err| format!("failed to read '{}': {}", config.path, err))?;

    if !config.logical
        && Path::new(&config.path)
            .extension()
            .and_then(|ext| ext.to_str())
            != Some("tfel")
    {
        eprintln!(
            "warning: '{}' does not use the .tfel extension (mirrored TFEL source is expected by default)",
            config.path
        );
    }

    let (logical_source, source_map) = if config.logical {
        (source.clone(), None)
    } else {
        let preprocessed = preprocess_source_with_map(&source);
        (preprocessed.source, Some(preprocessed.source_map))
    };

    let tokens = tokenize_with_options(
        &logical_source,
        LexOptions {
            strict_tfel: config.strict_tfel,
        },
    )
    .map_err(|errors| format_lex_errors(&logical_source, &source, source_map.as_ref(), &errors))?;

    let program = Parser::with_options(
        tokens,
        ParserOptions {
            strict_tfel: config.strict_tfel,
        },
    )
    .parse_program()
    .map_err(|errors| {
        format_parse_errors(&logical_source, &source, source_map.as_ref(), &errors)
    })?;

    let base_dir = Path::new(&config.path)
        .parent()
        .map(Path::to_path_buf)
        .unwrap_or_else(|| ".".into());
    let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    let options = EvaluatorOptions {
        strict_tfel: config.strict_tfel,
        permissions: RuntimePermissions {
            allow_fs: config.allow_fs,
            allow_net: config.allow_net,
        },
        module_search_paths: config
            .module_paths
            .into_iter()
            .map(PathBuf::from)
            .map(|path| {
                if path.is_absolute() {
                    path
                } else {
                    cwd.join(path)
                }
            })
            .collect(),
    };
    let mut evaluator = Evaluator::with_base_dir_and_options(base_dir, options);
    let value = evaluator
        .eval_program(&program)
        .map_err(|err| err.to_string())?;

    println!("{}", value);
    Ok(())
}

fn mirror_file(input: &str, output: &str) -> Result<(), String> {
    let source =
        fs::read_to_string(input).map_err(|err| format!("failed to read '{}': {}", input, err))?;
    let mirrored = preprocess_source(&source);
    fs::write(output, mirrored).map_err(|err| format!("failed to write '{}': {}", output, err))?;
    Ok(())
}

fn usage() -> String {
    [
        "usage:",
        "  tfel [--logical] [--strict-tfel] [--allow-fs] [--allow-net] [--module-path <dir>]... <path/to/file>",
        "  tfel mirror <logical-input> <mirrored-output>",
        "",
        "flags:",
        "  --logical      treat input as already-logical TFEL (skip mirror preprocessing)",
        "  --strict-tfel  disable compatibility syntax and enforce strict assignment behavior",
        "  --allow-fs     enable __read_file/__write_file/__delete_file builtins",
        "  --allow-net    enable __http_request builtin",
        "  --module-path  add a directory to module import search paths (repeatable)",
    ]
    .join("\n")
}

fn format_lex_errors(
    preprocessed_source: &str,
    original_source: &str,
    source_map: Option<&SourceMap>,
    errors: &[LexError],
) -> String {
    errors
        .iter()
        .map(|err| {
            format_error(
                preprocessed_source,
                original_source,
                source_map,
                err.span,
                &err.message,
                None,
            )
        })
        .collect::<Vec<_>>()
        .join("\n\n")
}

fn format_parse_errors(
    preprocessed_source: &str,
    original_source: &str,
    source_map: Option<&SourceMap>,
    errors: &[ParseError],
) -> String {
    errors
        .iter()
        .map(|err| {
            format_error(
                preprocessed_source,
                original_source,
                source_map,
                err.span,
                &err.message,
                err.hint.as_deref(),
            )
        })
        .collect::<Vec<_>>()
        .join("\n\n")
}

fn format_error(
    preprocessed_source: &str,
    original_source: &str,
    source_map: Option<&SourceMap>,
    span: Span,
    message: &str,
    hint: Option<&str>,
) -> String {
    let (source, mapped_span) = if let Some(map) = source_map {
        let (start, end) = map.map_span(span.start, span.end);
        (original_source, Span::new(start, end))
    } else {
        (preprocessed_source, span)
    };

    let (line_no, column_no, line_text) = line_context(source, mapped_span.start);
    let caret_padding = " ".repeat(column_no.saturating_sub(1));

    let mut rendered = format!(
        "{} at line {}, column {} (byte {}..{})\n{}\n{}^",
        message, line_no, column_no, mapped_span.start, mapped_span.end, line_text, caret_padding
    );

    if let Some(hint) = hint {
        rendered.push_str(&format!("\nhint: {hint}"));
    }

    rendered
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
