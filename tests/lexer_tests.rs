use tfel::lexer::{TokenKind, tokenize, tokenize_with_report};

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
