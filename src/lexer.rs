use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Eof,
    Ident(String),
    Number(f64),
    String(String),

    // Bootstrap keyword retained while parser is incrementally migrated.
    // Normal-language equivalent: this is the usual `let`.
    Let,
    Tel,
    Fed,
    Tropmi,
    Morf,
    Tropxe,
    Nruter,
    Break,
    Continue,
    If,
    Else,
    Rof,
    Ni,
    Elihw,
    Dna,
    Ro,
    Ton,
    Print,
    True,
    False,

    Assign,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Bang,
    Eq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,

    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexError {
    pub message: String,
    pub span: Span,
}

impl LexError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "lex error at {}..{}: {}",
            self.span.start, self.span.end, self.message
        )
    }
}

#[derive(Debug, Clone, Default)]
pub struct LexReport {
    pub tokens: Vec<Token>,
    pub errors: Vec<LexError>,
}

impl LexReport {
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}

pub fn tokenize(input: &str) -> Result<Vec<Token>, Vec<LexError>> {
    let report = tokenize_with_report(input);
    if report.has_errors() {
        Err(report.errors)
    } else {
        Ok(report.tokens)
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct LexOptions {
    pub strict_tfel: bool,
}

pub fn tokenize_with_options(
    input: &str,
    options: LexOptions,
) -> Result<Vec<Token>, Vec<LexError>> {
    let report = tokenize_with_report_options(input, options);
    if report.has_errors() {
        Err(report.errors)
    } else {
        Ok(report.tokens)
    }
}

pub fn tokenize_with_report(input: &str) -> LexReport {
    tokenize_with_report_options(input, LexOptions::default())
}

pub fn tokenize_with_report_options(input: &str, options: LexOptions) -> LexReport {
    let mut lexer = Lexer::new(input, options);
    let mut tokens = Vec::new();
    let mut errors = Vec::new();

    loop {
        match lexer.next_token() {
            Ok(token) => {
                let eof = matches!(token.kind, TokenKind::Eof);
                tokens.push(token);
                if eof {
                    break;
                }
            }
            Err(err) => errors.push(err),
        }
    }

    LexReport { tokens, errors }
}

struct Lexer<'a> {
    input: &'a str,
    position: usize,
    options: LexOptions,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str, options: LexOptions) -> Self {
        Self {
            input,
            position: 0,
            options,
        }
    }

    fn next_token(&mut self) -> Result<Token, LexError> {
        self.skip_ignored();

        let Some(raw) = self.peek_char() else {
            let span = Span::new(self.position, self.position);
            return Ok(Token::new(TokenKind::Eof, span));
        };

        let start = self.position;
        self.bump_char();

        if raw == '"' {
            return self.read_string(start);
        }

        if is_ident_start(raw) {
            return Ok(self.read_identifier(start));
        }

        if raw.is_ascii_digit() {
            return self.read_number(start);
        }

        let ch = swap_bracket(raw);

        let token = match ch {
            '=' => {
                if self.peek_char().map(swap_bracket) == Some('=') {
                    self.bump_char();
                    Token::new(TokenKind::Eq, Span::new(start, self.position))
                } else {
                    Token::new(TokenKind::Assign, Span::new(start, self.position))
                }
            }
            '!' => {
                if self.peek_char().map(swap_bracket) == Some('=') {
                    self.bump_char();
                    Token::new(TokenKind::NotEq, Span::new(start, self.position))
                } else {
                    Token::new(TokenKind::Bang, Span::new(start, self.position))
                }
            }
            '+' => Token::new(TokenKind::Plus, Span::new(start, self.position)),
            '-' => Token::new(TokenKind::Minus, Span::new(start, self.position)),
            '*' => Token::new(TokenKind::Star, Span::new(start, self.position)),
            '/' => Token::new(TokenKind::Slash, Span::new(start, self.position)),
            '%' => Token::new(TokenKind::Percent, Span::new(start, self.position)),
            '<' if self.peek_char().map(swap_bracket) == Some('=') => {
                self.bump_char();
                Token::new(TokenKind::LtEq, Span::new(start, self.position))
            }
            '>' if self.peek_char().map(swap_bracket) == Some('=') => {
                self.bump_char();
                Token::new(TokenKind::GtEq, Span::new(start, self.position))
            }
            '<' => Token::new(TokenKind::Lt, Span::new(start, self.position)),
            '>' => Token::new(TokenKind::Gt, Span::new(start, self.position)),
            ',' => Token::new(TokenKind::Comma, Span::new(start, self.position)),
            ';' => Token::new(TokenKind::Semicolon, Span::new(start, self.position)),
            '(' => Token::new(TokenKind::LParen, Span::new(start, self.position)),
            ')' => Token::new(TokenKind::RParen, Span::new(start, self.position)),
            '{' => Token::new(TokenKind::LBrace, Span::new(start, self.position)),
            '}' => Token::new(TokenKind::RBrace, Span::new(start, self.position)),
            '[' => Token::new(TokenKind::LBracket, Span::new(start, self.position)),
            ']' => Token::new(TokenKind::RBracket, Span::new(start, self.position)),
            other => {
                return Err(LexError::new(
                    format!("unexpected character '{}'", other),
                    Span::new(start, self.position),
                ));
            }
        };

        Ok(token)
    }

    fn read_identifier(&mut self, start: usize) -> Token {
        while self.peek_char().is_some_and(is_ident_continue) {
            self.bump_char();
        }

        let span = Span::new(start, self.position);
        let ident = &self.input[start..self.position];
        let strict = self.options.strict_tfel;
        // Most TFEL keywords are normal keywords mirrored in a spooky syntax mirror.
        let kind = match ident {
            "let" => TokenKind::Let,
            "tel" => TokenKind::Tel,
            "fed" => TokenKind::Fed,
            "tropmi" => TokenKind::Tropmi,
            "import" if !strict => TokenKind::Tropmi,
            "morf" => TokenKind::Morf,
            "from" if !strict => TokenKind::Morf,
            "tropxe" => TokenKind::Tropxe,
            "export" if !strict => TokenKind::Tropxe,
            "nruter" => TokenKind::Nruter,
            "kaerb" => TokenKind::Break,
            "break" if !strict => TokenKind::Break,
            "eunitnoc" => TokenKind::Continue,
            "continue" if !strict => TokenKind::Continue,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "rof" => TokenKind::Rof,
            "ni" => TokenKind::Ni,
            "elihw" => TokenKind::Elihw,
            "dna" => TokenKind::Dna,
            "ro" => TokenKind::Ro,
            "ton" => TokenKind::Ton,
            "print" => TokenKind::Print,
            "true" | "eurt" => TokenKind::True,
            "false" | "eslaf" => TokenKind::False,
            _ => TokenKind::Ident(ident.to_owned()),
        };

        Token::new(kind, span)
    }

    fn read_number(&mut self, start: usize) -> Result<Token, LexError> {
        let mut dot_seen = false;

        while let Some(c) = self.peek_char() {
            if c.is_ascii_digit() {
                self.bump_char();
                continue;
            }
            if c == '.' && !dot_seen {
                dot_seen = true;
                self.bump_char();
                continue;
            }
            break;
        }

        let span = Span::new(start, self.position);
        let raw = &self.input[start..self.position];
        let number = raw
            .parse::<f64>()
            .map_err(|_| LexError::new(format!("invalid number literal '{raw}'"), span))?;

        Ok(Token::new(TokenKind::Number(number), span))
    }

    fn read_string(&mut self, start: usize) -> Result<Token, LexError> {
        let mut value = String::new();

        while let Some(c) = self.peek_char() {
            self.bump_char();

            if c == '"' {
                return Ok(Token::new(
                    TokenKind::String(value),
                    Span::new(start, self.position),
                ));
            }

            if c == '\\' {
                let Some(esc) = self.peek_char() else {
                    return Err(LexError::new(
                        "unterminated escape sequence in string",
                        Span::new(start, self.position),
                    ));
                };
                self.bump_char();

                let escaped = match esc {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '\\' => '\\',
                    '"' => '"',
                    other => other,
                };
                value.push(escaped);
                continue;
            }

            value.push(c);
        }

        Err(LexError::new(
            "unterminated string literal",
            Span::new(start, self.position),
        ))
    }

    fn skip_ignored(&mut self) {
        loop {
            while self.peek_char().is_some_and(char::is_whitespace) {
                self.bump_char();
            }

            if self.peek_char() == Some('/') && self.peek_nth_char(1) == Some('/') {
                while let Some(c) = self.peek_char() {
                    self.bump_char();
                    if c == '\n' {
                        break;
                    }
                }
                continue;
            }

            break;
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.position..].chars().next()
    }

    fn peek_nth_char(&self, n: usize) -> Option<char> {
        self.input[self.position..].chars().nth(n)
    }

    fn bump_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.position += ch.len_utf8();
        Some(ch)
    }
}

fn swap_bracket(c: char) -> char {
    // Normal languages keep bracket shape.
    // TFEL swaps them so `(x)` in normal code is written as `)x(`.
    match c {
        '(' => ')',
        ')' => '(',
        '[' => ']',
        ']' => '[',
        '{' => '}',
        '}' => '{',
        _ => c,
    }
}

fn is_ident_start(c: char) -> bool {
    c == '_' || c.is_ascii_alphabetic()
}

fn is_ident_continue(c: char) -> bool {
    c == '_' || c == '.' || c.is_ascii_alphanumeric()
}
