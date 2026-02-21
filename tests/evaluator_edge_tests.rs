use std::fs;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};

use tfel::evaluator::{EvalError, Evaluator, Value};
use tfel::lexer::tokenize;
use tfel::parser::Parser;
use tfel::preprocessor::preprocess_source;

fn create_temp_dir(prefix: &str) -> PathBuf {
    static NEXT_ID: AtomicUsize = AtomicUsize::new(0);
    let id = NEXT_ID.fetch_add(1, Ordering::Relaxed);
    let dir = std::env::temp_dir().join(format!("tfel-edge-{prefix}-{}-{id}", std::process::id()));
    fs::create_dir_all(&dir).expect("temp dir creation should succeed");
    dir
}

fn write_module_preprocessed(path: &Path, logical_tfel: &str) {
    let encoded = preprocess_source(logical_tfel);
    fs::write(path, encoded).expect("module write should succeed");
}

fn eval_src(src: &str) -> Result<Value, EvalError> {
    let tokens = tokenize(src).expect("lexing should pass");
    let program = Parser::new(tokens)
        .parse_program()
        .expect("parsing should pass");
    let mut eval = Evaluator::new();
    eval.eval_program(&program)
}

fn eval_src_with_base(src: &str, base_dir: PathBuf) -> Result<Value, EvalError> {
    let tokens = tokenize(src).expect("lexing should pass");
    let program = Parser::new(tokens)
        .parse_program()
        .expect("parsing should pass");
    let mut eval = Evaluator::with_base_dir(base_dir);
    eval.eval_program(&program)
}

#[test]
fn unknown_variable_error_suggests_close_name() {
    let err = eval_src("10 = value; vaule;").expect_err("should fail for unknown variable");
    let rendered = format!("{err}");
    assert!(rendered.contains("unknown variable 'vaule'"));
    assert!(rendered.contains("did you mean 'value'?"));
}

#[test]
fn from_import_typo_suggests_export_name() {
    let dir = create_temp_dir("import-typo");
    write_module_preprocessed(&dir.join("math.tfel"), "40 = answer;\n");

    let err = eval_src_with_base("answr from math import;", dir.clone())
        .expect_err("should fail for missing import symbol");
    let rendered = format!("{err}");
    assert!(rendered.contains("does not export 'answr'"));
    assert!(rendered.contains("did you mean 'answer'?"));

    let _ = fs::remove_dir_all(dir);
}

#[test]
fn import_cycle_reports_clear_error() {
    let dir = create_temp_dir("import-cycle");
    write_module_preprocessed(&dir.join("a.tfel"), "\"b\" tropmi;\n1 = a_value;\n");
    write_module_preprocessed(&dir.join("b.tfel"), "\"a\" tropmi;\n2 = b_value;\n");

    let err = eval_src_with_base("\"a\" tropmi;", dir.clone())
        .expect_err("should fail for cyclic imports");
    assert!(err.message.contains("cyclic import detected"));

    let _ = fs::remove_dir_all(dir);
}

#[test]
fn module_folder_index_import_resolves_and_namespaces() {
    let dir = create_temp_dir("folder-index");
    let index_path = dir.join("lit/http/index.tfel");
    if let Some(parent) = index_path.parent() {
        fs::create_dir_all(parent).expect("module dir should be created");
    }
    write_module_preprocessed(&index_path, "5 = ok;\n");

    let value = eval_src_with_base("lit/http tropmi; http.ok;", dir.clone())
        .expect("folder index module should resolve");
    assert_eq!(value, Value::Number(5.0));

    let _ = fs::remove_dir_all(dir);
}

#[test]
fn while_guard_stops_infinite_loops() {
    let err = eval_src("elihw )eurt( } {").expect_err("should fail due iteration guard");
    assert!(err.message.contains("iteration limit"));
}

#[test]
fn return_outside_function_is_runtime_error() {
    let err = eval_src("nruter 10;").expect_err("should fail");
    assert!(err.message.contains("outside of a function"));
}

#[test]
fn range_step_zero_is_error_with_builtin_context() {
    let err = eval_src("range)0, 10, 0(;").expect_err("should fail");
    let rendered = format!("{err}");
    assert!(rendered.contains("range step cannot be zero"));
    assert!(rendered.contains("while calling builtin 'range'"));
}

#[test]
fn emit_time_rejects_invalid_format() {
    let err = eval_src("__emit_time)\"1430\"(;").expect_err("should fail for invalid time");
    assert!(err.message.contains("HH:MM"));
}

#[test]
fn http_request_rejects_invalid_scheme_without_network_call() {
    let err = eval_src("__http_request)\"GET\", \"ftp://example.com\"(;").expect_err("should fail");
    assert!(err.message.contains("scheme must be http or https"));
}

#[test]
fn file_read_missing_file_is_error() {
    let dir = create_temp_dir("file-missing");
    let err = eval_src_with_base("__read_file)\"missing.txt\"(;", dir.clone())
        .expect_err("missing file should fail");
    assert!(err.message.contains("failed to read file"));
    let _ = fs::remove_dir_all(dir);
}
