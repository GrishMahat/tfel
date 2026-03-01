use tfel::lexer::{LexOptions, TokenKind, tokenize, tokenize_with_options, tokenize_with_report};

#[test]
fn tokenizes_basic_program() {
    let src = "let x = 1 + 2 * 3;";
    let tokens = tokenize(src).expect("lexer should succeed");
    let kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();

    assert_eq!(
        kinds,
        vec![
            TokenKind::Let,
            TokenKind::Ident("x".to_string()),
            TokenKind::Assign,
            TokenKind::Number(1.0),
            TokenKind::Plus,
            TokenKind::Number(2.0),
            TokenKind::Star,
            TokenKind::Number(3.0),
            TokenKind::Semicolon,
            TokenKind::Eof
        ]
    );
}

#[test]
fn recognizes_import_alias_keywords() {
    let src = "import from";
    let tokens = tokenize(src).expect("lexer should succeed");
    let kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
    assert_eq!(
        kinds,
        vec![TokenKind::Tropmi, TokenKind::Morf, TokenKind::Eof]
    );
}

#[test]
fn tokenizes_dotted_identifiers_for_module_namespacing() {
    let src = "emit.lamron)\"14:30\"(;";
    let tokens = tokenize(src).expect("lexer should succeed");
    let kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
    assert_eq!(
        kinds,
        vec![
            TokenKind::Ident("emit.lamron".to_string()),
            TokenKind::LParen,
            TokenKind::String("14:30".to_string()),
            TokenKind::RParen,
            TokenKind::Semicolon,
            TokenKind::Eof
        ]
    );
}

#[test]
fn reports_unterminated_string() {
    let report = tokenize_with_report("\"uh oh");
    assert_eq!(report.errors.len(), 1);
    assert!(report.errors[0].message.contains("unterminated string"));
}

#[test]
fn tokenizes_less_equal_and_greater_equal() {
    let src = "1 <= 2 >= 1;";
    let tokens = tokenize(src).expect("lexer should succeed");
    let kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
    assert_eq!(
        kinds,
        vec![
            TokenKind::Number(1.0),
            TokenKind::LtEq,
            TokenKind::Number(2.0),
            TokenKind::GtEq,
            TokenKind::Number(1.0),
            TokenKind::Semicolon,
            TokenKind::Eof,
        ]
    );
}

#[test]
fn strict_mode_disables_import_alias_keywords() {
    let src = "import from export";
    let tokens =
        tokenize_with_options(src, LexOptions { strict_tfel: true }).expect("lexer should succeed");
    let kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
    assert_eq!(
        kinds,
        vec![
            TokenKind::Ident("import".to_string()),
            TokenKind::Ident("from".to_string()),
            TokenKind::Ident("export".to_string()),
            TokenKind::Eof
        ]
    );
}

#[test]
fn tokenizes_object_literal_colon_syntax() {
    let src = "}\"k\": 1{;";
    let tokens = tokenize(src).expect("lexer should succeed");
    let kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
    assert_eq!(
        kinds,
        vec![
            TokenKind::LBrace,
            TokenKind::String("k".to_string()),
            TokenKind::Colon,
            TokenKind::Number(1.0),
            TokenKind::RBrace,
            TokenKind::Semicolon,
            TokenKind::Eof
        ]
    );
}
