use std::fs;
use std::path::Path;
use std::sync::atomic::{AtomicUsize, Ordering};

use tfel::evaluator::{Evaluator, Value};
use tfel::lexer::tokenize;
use tfel::parser::Parser;
use tfel::preprocessor::preprocess_source;

fn create_temp_dir(prefix: &str) -> std::path::PathBuf {
    static NEXT_ID: AtomicUsize = AtomicUsize::new(0);
    let id = NEXT_ID.fetch_add(1, Ordering::Relaxed);
    let dir = std::env::temp_dir().join(format!("tfel-it-{prefix}-{}-{id}", std::process::id()));
    fs::create_dir_all(&dir).expect("temp dir creation should succeed");
    dir
}

fn write_module_preprocessed(path: &Path, preprocessed: &str) {
    let encoded = preprocess_source(preprocessed);
    fs::write(path, encoded).expect("module write should succeed");
}

#[test]
fn evaluates_tfel_assignment_shape() {
    let tokens = tokenize("10 = x; x + 5;").expect("lexing should pass");
    let program = Parser::new(tokens)
        .parse_program()
        .expect("parser should pass");

    let mut eval = Evaluator::new();
    let value = eval.eval_program(&program).expect("evaluation should pass");
    assert_eq!(value, Value::Number(15.0));
}

#[test]
fn supports_string_indexing() {
    let tokens = tokenize("\"tfel\"]1[ + \"-\" + \"tfel\"]-1[;").expect("lexing should pass");
    let program = Parser::new(tokens)
        .parse_program()
        .expect("parser should pass");

    let mut eval = Evaluator::new();
    let value = eval.eval_program(&program).expect("evaluation should pass");
    assert_eq!(value, Value::String("f-l".to_string()));
}

#[test]
fn import_exposes_namespaced_symbols() {
    let dir = create_temp_dir("imports-namespace");
    let module_path = dir.join("math.tfel");
    write_module_preprocessed(&module_path, "40 = answer;\n");

    let tokens = tokenize("\"math\" tropmi; math.answer;").expect("lexing should pass");
    let program = Parser::new(tokens)
        .parse_program()
        .expect("parser should pass");

    let mut eval = Evaluator::with_base_dir(dir.clone());
    let value = eval.eval_program(&program).expect("evaluation should pass");
    assert_eq!(value, Value::Number(40.0));

    let _ = fs::remove_dir_all(dir);
}

#[test]
fn file_library_round_trip_write_read_delete() {
    let dir = create_temp_dir("file-library");
    let lib_dir = dir.join("lib");
    fs::create_dir_all(&lib_dir).expect("nested module dir should be created");
    write_module_preprocessed(
        &lib_dir.join("file.tfel"),
        "fed read)path( } nruter __read_file)path(; {\n\
         fed write)path, content( } nruter __write_file)path, content(; {\n\
         fed delete)path( } nruter __delete_file)path(; {\n",
    );

    let tokens = tokenize(
        "lib/file tropmi; \
         file.write)\"tmp/demo.txt\", \"cursed\"(; \
         file.read)\"tmp/demo.txt\"(;",
    )
    .expect("lexing should pass");
    let program = Parser::new(tokens)
        .parse_program()
        .expect("parser should pass");

    let mut eval = Evaluator::with_base_dir(dir.clone());
    let value = eval.eval_program(&program).expect("evaluation should pass");
    assert_eq!(value, Value::String("cursed".to_string()));

    let cleanup_tokens =
        tokenize("lib/file tropmi; file.delete)\"tmp/demo.txt\"(;").expect("lexing should pass");
    let cleanup_program = Parser::new(cleanup_tokens)
        .parse_program()
        .expect("parser should pass");
    let deleted = eval
        .eval_program(&cleanup_program)
        .expect("delete should succeed");
    assert_eq!(deleted, Value::Boolean(true));

    let _ = fs::remove_dir_all(dir);
}
