use std::fs;
use std::path::{Path, PathBuf};

use tfel::evaluator::{Evaluator, EvaluatorOptions, RuntimePermissions};
use tfel::lexer::tokenize;
use tfel::parser::Parser;
use tfel::preprocessor::preprocess_source;

fn run_example(path: &Path) -> Result<String, String> {
    let source = fs::read_to_string(path)
        .map_err(|err| format!("failed to read '{}': {}", path.display(), err))?;
    let preprocessed = preprocess_source(&source);
    let tokens = tokenize(&preprocessed)
        .map_err(|errors| format!("lex error in '{}': {}", path.display(), errors[0].message))?;
    let program = Parser::new(tokens)
        .parse_program()
        .map_err(|errors| format!("parse error in '{}': {}", path.display(), errors[0].message))?;

    let base_dir = path
        .parent()
        .unwrap_or_else(|| Path::new("."))
        .to_path_buf();
    let mut evaluator = Evaluator::with_base_dir_and_options(
        base_dir,
        EvaluatorOptions {
            strict_tfel: false,
            permissions: RuntimePermissions {
                allow_fs: true,
                allow_net: false,
            },
            module_search_paths: Vec::new(),
        },
    );
    evaluator
        .eval_program(&program)
        .map(|value| value.to_string())
        .map_err(|err| format!("runtime error in '{}': {err}", path.display()))
}

fn top_level_example_files(root: &Path) -> Vec<PathBuf> {
    let mut files = fs::read_dir(root)
        .expect("examples directory should exist")
        .filter_map(Result::ok)
        .map(|entry| entry.path())
        .filter(|path| path.extension().and_then(|ext| ext.to_str()) == Some("tfel"))
        .collect::<Vec<_>>();
    files.sort();
    files
}

#[test]
fn all_top_level_examples_lex_and_parse() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples");

    for path in top_level_example_files(&root) {
        let source = fs::read_to_string(&path)
            .unwrap_or_else(|err| panic!("failed to read '{}': {}", path.display(), err));
        let preprocessed = preprocess_source(&source);
        let tokens = tokenize(&preprocessed)
            .unwrap_or_else(|errors| panic!("lex error in '{}': {}", path.display(), errors[0]));
        let parsed = Parser::new(tokens).parse_program();
        assert!(
            parsed.is_ok(),
            "parse error in '{}': {}",
            path.display(),
            parsed.err().unwrap()[0]
        );
    }
}

#[test]
fn non_network_examples_execute_without_errors() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples");
    let expected_failures = ["error_hint_demo.tfel", "error_context_demo.tfel"];
    let runtime_skips = [
        "api_request_demo.tfel",
        "http_demo.tfel",
        "input_demo.tfel",
        "fibonacci.tfel",
        "service_health_reporter.tfel",
    ];

    for path in top_level_example_files(&root) {
        let name = path
            .file_name()
            .and_then(|file| file.to_str())
            .expect("example name should be UTF-8");

        if expected_failures.contains(&name) || runtime_skips.contains(&name) {
            continue;
        }

        let result = run_example(&path);
        assert!(result.is_ok(), "{} failed: {:?}", name, result.err());
    }
}

#[test]
fn intentional_error_examples_fail_with_diagnostics() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples");

    let hint_err = run_example(&root.join("error_hint_demo.tfel"))
        .expect_err("error_hint_demo should intentionally fail");
    assert!(hint_err.contains("did you mean"));

    let context_err = run_example(&root.join("error_context_demo.tfel"))
        .expect_err("error_context_demo should intentionally fail");
    assert!(context_err.contains("context:"));
}
