use tfel::ast::{Expr, Stmt};
use tfel::lexer::tokenize;
use tfel::parser::Parser;

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
