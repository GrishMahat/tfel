use tfel::ast::{Expr, Stmt};
use tfel::lexer::tokenize;
use tfel::parser::{Parser, ParserOptions};

#[test]
fn parses_tfel_right_to_left_assignment() {
    let src = "10 + 5 = total;";
    let tokens = tokenize(src).expect("lexer should succeed");
    let program = Parser::new(tokens)
        .parse_program()
        .expect("parser should succeed");

    match &program.statements[0] {
        Stmt::Assign { name, .. } => assert_eq!(name, "total"),
        _ => panic!("expected assignment statement"),
    }
}

#[test]
fn parses_import_module_path_without_quotes() {
    let src = "lib/lint tropmi;";
    let tokens = tokenize(src).expect("lexer should succeed");
    let program = Parser::new(tokens)
        .parse_program()
        .expect("parser should succeed");

    assert_eq!(
        program.statements[0],
        Stmt::Import {
            module: "lib/lint".to_string(),
            item: None
        }
    );
}

#[test]
fn parses_from_import_with_bare_identifiers() {
    let src = "sat morf lib/lint tropmi;";
    let tokens = tokenize(src).expect("lexer should succeed");
    let program = Parser::new(tokens)
        .parse_program()
        .expect("parser should succeed");

    assert_eq!(
        program.statements[0],
        Stmt::Import {
            module: "lib/lint".to_string(),
            item: Some("sat".to_string())
        }
    );
}

#[test]
fn parses_namespaced_call_expression() {
    let src = "emit.lamron)\"14:30\"(;";
    let tokens = tokenize(src).expect("lexer should succeed");
    let program = Parser::new(tokens)
        .parse_program()
        .expect("parser should succeed");

    match &program.statements[0] {
        Stmt::Expr(Expr::Call { callee, args }) => {
            assert_eq!(
                callee.as_ref(),
                &Expr::Identifier("emit.lamron".to_string())
            );
            assert_eq!(args, &vec![Expr::String("14:30".to_string())]);
        }
        _ => panic!("expected namespaced call expression"),
    }
}

#[test]
fn parser_reports_hint_for_normal_assignment_shape() {
    let src = "x = 10;";
    let tokens = tokenize(src).expect("lexer should succeed");
    let errors = Parser::new(tokens)
        .parse_program()
        .expect_err("parser should fail");

    assert_eq!(errors.len(), 1);
    assert!(errors[0].message.contains("TFEL: value = name"));
    assert!(
        errors[0]
            .hint
            .as_deref()
            .unwrap_or_default()
            .contains("normal assignment")
    );
}

#[test]
fn strict_mode_rejects_let_keyword_with_hint() {
    let src = "let x = 10;";
    let tokens = tokenize(src).expect("lexer should succeed");
    let errors = Parser::with_options(tokens, ParserOptions { strict_tfel: true })
        .parse_program()
        .expect_err("parser should reject let in strict mode");

    assert_eq!(errors.len(), 1);
    assert!(errors[0].message.contains("disabled in --strict-tfel"));
    assert_eq!(
        errors[0].hint.as_deref(),
        Some("use `tel name = value;` instead")
    );
}
