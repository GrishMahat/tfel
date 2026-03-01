use std::fs;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};

use tfel::evaluator::{EvalError, Evaluator, EvaluatorOptions, RuntimePermissions, Value};
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
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).expect("module parent dir creation should succeed");
    }
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

fn eval_src_with_options(src: &str, options: EvaluatorOptions) -> Result<Value, EvalError> {
    let tokens = tokenize(src).expect("lexing should pass");
    let program = Parser::new(tokens)
        .parse_program()
        .expect("parsing should pass");
    let mut eval = Evaluator::with_options(options);
    eval.eval_program(&program)
}

fn eval_src_with_base_and_options(
    src: &str,
    base_dir: PathBuf,
    options: EvaluatorOptions,
) -> Result<Value, EvalError> {
    let tokens = tokenize(src).expect("lexing should pass");
    let program = Parser::new(tokens)
        .parse_program()
        .expect("parsing should pass");
    let mut eval = Evaluator::with_base_dir_and_options(base_dir, options);
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
    let err = eval_src_with_options(
        "__http_request)\"GET\", \"ftp://example.com\"(;",
        EvaluatorOptions {
            strict_tfel: false,
            permissions: RuntimePermissions {
                allow_fs: false,
                allow_net: true,
            },
            module_search_paths: Vec::new(),
        },
    )
    .expect_err("should fail");
    assert!(err.message.contains("scheme must be http or https"));
}

#[test]
fn http_request_error_includes_phase_url_and_target_on_resolve_failure() {
    let err = eval_src_with_options(
        "__http_request)\"GET\", \"http://definitely-missing-host.invalid\"(;",
        EvaluatorOptions {
            strict_tfel: false,
            permissions: RuntimePermissions {
                allow_fs: false,
                allow_net: true,
            },
            module_search_paths: Vec::new(),
        },
    )
    .expect_err("missing host should fail");
    let rendered = format!("{err}");
    assert!(rendered.contains("phase=resolve"));
    assert!(rendered.contains("url=http://definitely-missing-host.invalid"));
    assert!(rendered.contains("target=definitely-missing-host.invalid:80"));
}

#[test]
fn file_read_missing_file_is_error() {
    let dir = create_temp_dir("file-missing");
    let err = eval_src_with_base_and_options(
        "__read_file)\"missing.txt\"(;",
        dir.clone(),
        EvaluatorOptions {
            strict_tfel: false,
            permissions: RuntimePermissions {
                allow_fs: true,
                allow_net: false,
            },
            module_search_paths: Vec::new(),
        },
    )
    .expect_err("missing file should fail");
    assert!(err.message.contains("failed to read file"));
    let _ = fs::remove_dir_all(dir);
}

#[test]
fn recursion_depth_limit_returns_runtime_error() {
    let err = eval_src("fed recurse)n( } nruter recurse)n + 1(; { recurse)0(;")
        .expect_err("deep recursion should fail with a runtime error");
    let rendered = format!("{err}");
    assert!(rendered.contains("call depth exceeded limit"));
}

#[test]
fn strict_mode_rejects_assignment_to_undefined_variable() {
    let err = eval_src_with_options(
        "20 = y;",
        EvaluatorOptions {
            strict_tfel: true,
            permissions: RuntimePermissions::default(),
            module_search_paths: Vec::new(),
        },
    )
    .expect_err("strict mode should reject implicit variable creation");
    assert!(err.message.contains("assignment to undefined variable 'y'"));
}

#[test]
fn default_runtime_permissions_are_restricted() {
    let fs_err =
        eval_src("__read_file)\"foo.txt\"(;").expect_err("default runtime should block fs");
    assert!(fs_err.message.contains("filesystem access is disabled"));

    let net_err = eval_src("__http_request)\"GET\", \"https://example.com\"(;")
        .expect_err("default runtime should block network");
    assert!(net_err.message.contains("network access is disabled"));
}

#[test]
fn permission_gate_blocks_filesystem_builtins() {
    let err = eval_src_with_options(
        "__read_file)\"foo.txt\"(;",
        EvaluatorOptions {
            strict_tfel: false,
            permissions: RuntimePermissions::restricted(),
            module_search_paths: Vec::new(),
        },
    )
    .expect_err("restricted runtime should block fs");
    assert!(err.message.contains("filesystem access is disabled"));
}

#[test]
fn permission_gate_blocks_network_builtin() {
    let err = eval_src_with_options(
        "__http_request)\"GET\", \"https://example.com\"(;",
        EvaluatorOptions {
            strict_tfel: false,
            permissions: RuntimePermissions::restricted(),
            module_search_paths: Vec::new(),
        },
    )
    .expect_err("restricted runtime should block network");
    assert!(err.message.contains("network access is disabled"));
}

#[test]
fn import_missing_module_lists_searched_candidates() {
    let dir = create_temp_dir("import-missing");
    let err = eval_src_with_base("\"missing/mod\" tropmi;", dir.clone())
        .expect_err("missing module should fail");
    let rendered = format!("{err}");
    assert!(rendered.contains("module 'missing/mod' was not found"));
    assert!(rendered.contains("searched paths:"));
    assert!(rendered.contains("missing/mod.tfel"));
    assert!(rendered.contains("missing/mod/mod.tfel"));
    assert!(rendered.contains("missing/mod/index.tfel"));
    let _ = fs::remove_dir_all(dir);
}

#[test]
fn module_search_paths_are_used_for_import_resolution() {
    let project_dir = create_temp_dir("module-path-project");
    let module_root = create_temp_dir("module-path-lib");
    write_module_preprocessed(&module_root.join("shared/math.tfel"), "40 = answer;\n");

    let value = eval_src_with_base_and_options(
        "\"shared/math\" tropmi; math.answer;",
        project_dir.clone(),
        EvaluatorOptions {
            strict_tfel: false,
            permissions: RuntimePermissions::restricted(),
            module_search_paths: vec![module_root.clone()],
        },
    )
    .expect("import should resolve from additional module search path");
    assert_eq!(value, Value::Number(40.0));

    let _ = fs::remove_dir_all(project_dir);
    let _ = fs::remove_dir_all(module_root);
}

#[test]
fn object_index_requires_string_key() {
    let err = eval_src("}\"a\": 1{]0[;").expect_err("object keys must be strings");
    assert!(err.message.contains("object key must be a string"));
}

#[test]
fn missing_object_key_reports_error() {
    let err = eval_src("}\"a\": 1{]\"b\"[;").expect_err("missing object key should fail");
    assert!(err.message.contains("object key 'b' not found"));
}

#[test]
fn interpolation_reports_unclosed_expression() {
    let err = eval_src("\"value=${1 + 2\";").expect_err("unterminated interpolation should fail");
    let rendered = format!("{err}");
    assert!(rendered.contains("unterminated string interpolation"));
}
