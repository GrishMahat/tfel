use std::fmt;

use crate::ast::{Expr, InfixOp, PrefixOp, Program, Stmt};
use crate::lexer::{Span, Token, TokenKind};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
    pub hint: Option<String>,
}

impl ParseError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
            hint: None,
        }
    }

    pub fn with_hint(mut self, hint: impl Into<String>) -> Self {
        if self.hint.is_none() {
            self.hint = Some(hint.into());
        }
        self
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "parse error at {}..{}: {}",
            self.span.start, self.span.end, self.message
        )?;
        if let Some(hint) = &self.hint {
            write!(f, "\n  hint: {hint}")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct ParserOptions {
    pub strict_tfel: bool,
}

pub struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
    options: ParserOptions,
}

enum ImportAtom {
    String(String),
    Ident(String),
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self::with_options(tokens, ParserOptions::default())
    }

    pub fn with_options(mut tokens: Vec<Token>, options: ParserOptions) -> Self {
        if !tokens
            .last()
            .is_some_and(|token| matches!(token.kind, TokenKind::Eof))
        {
            tokens.push(Token::new(TokenKind::Eof, Span::default()));
        }

        Self {
            tokens,
            cursor: 0,
            options,
        }
    }

    pub fn parse_program(mut self) -> Result<Program, Vec<ParseError>> {
        let mut statements = Vec::new();
        let mut errors = Vec::new();

        while !self.at_end() {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => {
                    errors.push(err);
                    self.synchronize();
                }
            }
        }

        if errors.is_empty() {
            Ok(Program::new(statements))
        } else {
            Err(errors)
        }
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        if self.check(|kind| matches!(kind, TokenKind::Let | TokenKind::Tel)) {
            self.parse_let_statement()
        } else if self.is_import_statement_start() {
            self.parse_import_statement()
        } else if self.check(|kind| matches!(kind, TokenKind::Tropxe)) {
            self.parse_export_statement()
        } else if self.check(|kind| matches!(kind, TokenKind::Print)) {
            self.parse_print_statement()
        } else if self.check(|kind| matches!(kind, TokenKind::If)) {
            self.parse_if_statement()
        } else if self.check(|kind| matches!(kind, TokenKind::Elihw)) {
            self.parse_while_statement()
        } else if self.check(|kind| matches!(kind, TokenKind::Rof)) {
            self.parse_for_statement()
        } else if self.check(|kind| matches!(kind, TokenKind::Fed)) {
            self.parse_function_definition()
        } else if self.check(|kind| matches!(kind, TokenKind::Nruter)) {
            self.parse_return_statement()
        } else if self.check(|kind| matches!(kind, TokenKind::Break)) {
            self.parse_break_statement()
        } else if self.check(|kind| matches!(kind, TokenKind::Continue)) {
            self.parse_continue_statement()
        } else {
            self.parse_assignment_or_expression_statement()
        }
    }

    fn parse_let_statement(&mut self) -> Result<Stmt, ParseError> {
        let keyword = self.advance();
        if matches!(keyword.kind, TokenKind::Let) && self.options.strict_tfel {
            return Err(
                ParseError::new("`let` is disabled in --strict-tfel mode", keyword.span)
                    .with_hint("use `tel name = value;` instead"),
            );
        }

        let name = self.expect_ident()?;
        self.expect(
            |kind| matches!(kind, TokenKind::Assign),
            "expected '=' after identifier",
        )?;

        let value = self.parse_expression(Precedence::Lowest)?;
        self.consume_semicolon();

        Ok(Stmt::Let { name, value })
    }

    fn parse_break_statement(&mut self) -> Result<Stmt, ParseError> {
        self.advance();
        self.consume_semicolon();
        Ok(Stmt::Break)
    }

    fn parse_continue_statement(&mut self) -> Result<Stmt, ParseError> {
        self.advance();
        self.consume_semicolon();
        Ok(Stmt::Continue)
    }

    fn parse_export_statement(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume `tropxe`

        let mut names = Vec::new();
        loop {
            names.push(self.expect_ident_with_message("expected identifier after 'tropxe'")?);

            if self.check(|kind| matches!(kind, TokenKind::Comma)) {
                self.advance();
                continue;
            }
            break;
        }

        self.consume_semicolon();
        Ok(Stmt::Export { names })
    }

    fn parse_print_statement(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume `print`
        self.expect(
            |kind| matches!(kind, TokenKind::LParen),
            "expected '(' after print",
        )?;
        let value = self.parse_expression(Precedence::Lowest)?;
        self.expect(
            |kind| matches!(kind, TokenKind::RParen),
            "expected ')' after print argument",
        )?;
        self.consume_semicolon();

        Ok(Stmt::Print { value })
    }

    fn parse_assignment_or_expression_statement(&mut self) -> Result<Stmt, ParseError> {
        let value = self.parse_expression(Precedence::Lowest)?;

        if self.check(|kind| matches!(kind, TokenKind::Assign)) {
            self.advance(); // consume '='

            let rhs = self.current().clone();
            let name = match rhs.kind {
                TokenKind::Ident(name) => {
                    self.advance();
                    name
                }
                _ => {
                    let mut err = ParseError::new(
                        "expected identifier on right side of assignment (TFEL: value = name)",
                        rhs.span,
                    );
                    if let Expr::Identifier(lhs) = &value {
                        err = err.with_hint(format!(
                            "this looks like normal assignment. TFEL uses `value = name`; try `<value> = {lhs};`"
                        ));
                    }
                    return Err(err);
                }
            };

            self.consume_semicolon();
            return Ok(Stmt::Assign { name, value });
        }

        self.consume_semicolon();
        Ok(Stmt::Expr(value))
    }

    fn parse_import_statement(&mut self) -> Result<Stmt, ParseError> {
        let first =
            self.parse_import_atom("expected module path or item name at import statement start")?;

        if self.check(|kind| matches!(kind, TokenKind::Morf)) {
            self.advance(); // consume `morf`
            let module = self.parse_module_path("expected module path after 'morf'")?;
            self.expect(
                |kind| matches!(kind, TokenKind::Tropmi),
                "expected 'tropmi' after module path",
            )?;
            self.consume_semicolon();
            return Ok(Stmt::Import {
                module,
                item: Some(import_atom_into_string(first)),
            });
        }

        let module = self.finish_module_path(first, "expected module path segment after '/'")?;
        self.expect(
            |kind| matches!(kind, TokenKind::Tropmi),
            "expected 'tropmi' after module path",
        )?;
        self.consume_semicolon();
        Ok(Stmt::Import { module, item: None })
    }

    fn parse_if_statement(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume `if`
        self.expect(
            |kind| matches!(kind, TokenKind::LParen),
            "expected '(' after if",
        )?;
        let condition = self.parse_expression(Precedence::Lowest)?;
        self.expect(
            |kind| matches!(kind, TokenKind::RParen),
            "expected ')' after if condition",
        )?;

        let then_branch = self.parse_block("expected block after if condition")?;

        let else_branch = if self.check(|kind| matches!(kind, TokenKind::Else)) {
            self.advance(); // consume `else`
            Some(self.parse_block("expected block after else")?)
        } else {
            None
        };

        Ok(Stmt::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn parse_while_statement(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume `elihw`
        self.expect(
            |kind| matches!(kind, TokenKind::LParen),
            "expected '(' after while",
        )?;
        let condition = self.parse_expression(Precedence::Lowest)?;
        self.expect(
            |kind| matches!(kind, TokenKind::RParen),
            "expected ')' after while condition",
        )?;
        let body = self.parse_block("expected block after while condition")?;
        Ok(Stmt::While { condition, body })
    }

    fn parse_for_statement(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume `rof`
        let name = self.expect_ident_with_message("expected loop variable after 'rof'")?;
        self.expect(
            |kind| matches!(kind, TokenKind::Ni),
            "expected 'ni' in for statement",
        )?;
        let iterable = self.parse_expression(Precedence::Lowest)?;
        let body = self.parse_block("expected block after for iterable expression")?;
        Ok(Stmt::For {
            name,
            iterable,
            body,
        })
    }

    fn parse_function_definition(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume `fed`
        let name = self.expect_ident_with_message("expected function name after 'fed'")?;
        self.expect(
            |kind| matches!(kind, TokenKind::LParen),
            "expected '(' after function name",
        )?;

        let mut params = Vec::new();
        if !self.check(|kind| matches!(kind, TokenKind::RParen)) {
            loop {
                params.push(
                    self.expect_ident_with_message(
                        "expected parameter name in function definition",
                    )?,
                );

                if self.check(|kind| matches!(kind, TokenKind::Comma)) {
                    self.advance();
                    continue;
                }
                break;
            }
        }

        self.expect(
            |kind| matches!(kind, TokenKind::RParen),
            "expected ')' after function parameters",
        )?;

        let body = self.parse_block("expected function body block")?;
        Ok(Stmt::FunctionDef { name, params, body })
    }

    fn parse_return_statement(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume `nruter`
        if self.check(|kind| matches!(kind, TokenKind::Semicolon)) {
            self.advance();
            return Ok(Stmt::Return(None));
        }

        if self.check(|kind| matches!(kind, TokenKind::RBrace)) || self.at_end() {
            return Ok(Stmt::Return(None));
        }

        let value = self.parse_expression(Precedence::Lowest)?;
        self.consume_semicolon();
        Ok(Stmt::Return(Some(value)))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expr, ParseError> {
        // Same Pratt parser strategy you would use in a normal interpreter.
        // The cursed part is mostly in preprocessing + token shapes, not precedence math.
        let mut left = self.parse_prefix()?;

        while !self.at_end()
            && !self.check(|kind| matches!(kind, TokenKind::Semicolon))
            && precedence < self.current_precedence()
        {
            let operator = self.advance();
            left = self.parse_infix(left, operator.kind)?;
        }

        Ok(left)
    }

    fn parse_prefix(&mut self) -> Result<Expr, ParseError> {
        let token = self.advance();
        match token.kind {
            TokenKind::Ident(name) => Ok(Expr::Identifier(name)),
            TokenKind::Number(value) => Ok(Expr::Number(value)),
            TokenKind::String(value) => Ok(Expr::String(value)),
            TokenKind::True => Ok(Expr::Boolean(true)),
            TokenKind::False => Ok(Expr::Boolean(false)),
            TokenKind::Ton => {
                let rhs = self.parse_expression(Precedence::Prefix)?;
                Ok(Expr::Prefix {
                    op: PrefixOp::Not,
                    rhs: Box::new(rhs),
                })
            }
            TokenKind::Bang => {
                let rhs = self.parse_expression(Precedence::Prefix)?;
                Ok(Expr::Prefix {
                    op: PrefixOp::Not,
                    rhs: Box::new(rhs),
                })
            }
            TokenKind::Minus => {
                let rhs = self.parse_expression(Precedence::Prefix)?;
                Ok(Expr::Prefix {
                    op: PrefixOp::Negate,
                    rhs: Box::new(rhs),
                })
            }
            TokenKind::LParen => {
                let expr = self.parse_expression(Precedence::Lowest)?;
                self.expect(
                    |kind| matches!(kind, TokenKind::RParen),
                    "expected ')' after grouped expression",
                )?;
                Ok(expr)
            }
            TokenKind::LBracket => self.parse_array_literal(),
            _ => {
                let mut err = ParseError::new(
                    format!(
                        "expected expression, found {}",
                        describe_token_kind(&token.kind)
                    ),
                    token.span,
                );
                if matches!(token.kind, TokenKind::Assign) {
                    err = err.with_hint(
                        "assignment in TFEL is mirrored: write `value = name`, not `name = value`",
                    );
                }
                Err(err)
            }
        }
    }

    fn parse_infix(&mut self, lhs: Expr, operator: TokenKind) -> Result<Expr, ParseError> {
        if matches!(operator, TokenKind::LBracket) {
            return self.parse_index_expression(lhs);
        }
        if matches!(operator, TokenKind::LParen) {
            return self.parse_call_expression(lhs);
        }

        let (op, precedence) = match operator {
            TokenKind::Dna => (InfixOp::And, Precedence::LogicalAnd),
            TokenKind::Ro => (InfixOp::Or, Precedence::LogicalOr),
            TokenKind::Plus => (InfixOp::Add, Precedence::Sum),
            TokenKind::Minus => (InfixOp::Subtract, Precedence::Sum),
            TokenKind::Star => (InfixOp::Multiply, Precedence::Product),
            TokenKind::Slash => (InfixOp::Divide, Precedence::Product),
            TokenKind::Percent => (InfixOp::Modulo, Precedence::Product),
            TokenKind::Eq => (InfixOp::Eq, Precedence::Equality),
            TokenKind::NotEq => (InfixOp::NotEq, Precedence::Equality),
            TokenKind::Lt => (InfixOp::Lt, Precedence::Comparison),
            TokenKind::Gt => (InfixOp::Gt, Precedence::Comparison),
            TokenKind::LtEq => (InfixOp::LtEq, Precedence::Comparison),
            TokenKind::GtEq => (InfixOp::GtEq, Precedence::Comparison),
            _ => {
                return Err(ParseError::new(
                    "expected infix operator",
                    self.current().span,
                ));
            }
        };

        let rhs = self.parse_expression(precedence)?;
        Ok(Expr::Infix {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
        })
    }

    fn parse_call_expression(&mut self, callee: Expr) -> Result<Expr, ParseError> {
        let mut args = Vec::new();

        if self.check(|kind| matches!(kind, TokenKind::RParen)) {
            self.advance();
            return Ok(Expr::Call {
                callee: Box::new(callee),
                args,
            });
        }

        loop {
            args.push(self.parse_expression(Precedence::Lowest)?);

            if self.check(|kind| matches!(kind, TokenKind::Comma)) {
                self.advance();
                continue;
            }

            self.expect(
                |kind| matches!(kind, TokenKind::RParen),
                "expected ')' after function call arguments",
            )?;
            break;
        }

        Ok(Expr::Call {
            callee: Box::new(callee),
            args,
        })
    }

    fn parse_block(&mut self, missing_open_message: &'static str) -> Result<Vec<Stmt>, ParseError> {
        self.expect(
            |kind| matches!(kind, TokenKind::LBrace),
            missing_open_message,
        )?;

        let mut statements = Vec::new();
        while !self.at_end() && !self.check(|kind| matches!(kind, TokenKind::RBrace)) {
            statements.push(self.parse_statement()?);
        }

        self.expect(
            |kind| matches!(kind, TokenKind::RBrace),
            "expected '}' to close block",
        )?;

        Ok(statements)
    }

    fn is_import_statement_start(&self) -> bool {
        if !matches!(
            self.current().kind,
            TokenKind::String(_) | TokenKind::Ident(_)
        ) {
            return false;
        }

        let mut offset = 1usize;
        loop {
            match self.peek_kind(offset) {
                Some(TokenKind::Tropmi) => return true,
                Some(TokenKind::Semicolon | TokenKind::RBrace | TokenKind::Eof) => return false,
                Some(_) => offset += 1,
                None => return false,
            }
        }
    }

    fn parse_array_literal(&mut self) -> Result<Expr, ParseError> {
        let mut items = Vec::new();

        if self.check(|kind| matches!(kind, TokenKind::RBracket)) {
            self.advance();
            return Ok(Expr::Array(items));
        }

        loop {
            items.push(self.parse_expression(Precedence::Lowest)?);

            if self.check(|kind| matches!(kind, TokenKind::Comma)) {
                self.advance();
                continue;
            }

            self.expect(
                |kind| matches!(kind, TokenKind::RBracket),
                "expected ']' after array literal",
            )?;
            break;
        }

        Ok(Expr::Array(items))
    }

    fn parse_index_expression(&mut self, target: Expr) -> Result<Expr, ParseError> {
        let index = self.parse_expression(Precedence::Lowest)?;
        self.expect(
            |kind| matches!(kind, TokenKind::RBracket),
            "expected ']' after index expression",
        )?;

        Ok(Expr::Index {
            target: Box::new(target),
            index: Box::new(index),
        })
    }

    fn expect_ident(&mut self) -> Result<String, ParseError> {
        self.expect_ident_with_message("expected identifier")
    }

    fn parse_import_atom(&mut self, message: &'static str) -> Result<ImportAtom, ParseError> {
        let token = self.advance();
        match token.kind {
            TokenKind::String(value) => Ok(ImportAtom::String(value)),
            TokenKind::Ident(name) => Ok(ImportAtom::Ident(name)),
            _ => Err(ParseError::new(message, token.span)),
        }
    }

    fn parse_module_path(&mut self, message: &'static str) -> Result<String, ParseError> {
        let first = self.parse_import_atom(message)?;
        self.finish_module_path(first, "expected module path segment after '/'")
    }

    fn finish_module_path(
        &mut self,
        first: ImportAtom,
        segment_error_message: &'static str,
    ) -> Result<String, ParseError> {
        match first {
            ImportAtom::String(path) => Ok(path),
            ImportAtom::Ident(first_segment) => {
                let mut path = first_segment;
                while self.check(|kind| matches!(kind, TokenKind::Slash)) {
                    self.advance(); // consume `/`
                    let segment = self.expect_ident_with_message(segment_error_message)?;
                    path.push('/');
                    path.push_str(&segment);
                }
                Ok(path)
            }
        }
    }

    fn expect_ident_with_message(&mut self, message: &'static str) -> Result<String, ParseError> {
        let token = self.advance();
        match token.kind {
            TokenKind::Ident(name) => Ok(name),
            _ => Err(ParseError::new(message, token.span)),
        }
    }

    fn expect(
        &mut self,
        predicate: impl Fn(&TokenKind) -> bool,
        message: &'static str,
    ) -> Result<(), ParseError> {
        if predicate(&self.current().kind) {
            self.advance();
            Ok(())
        } else {
            Err(ParseError::new(message, self.current().span))
        }
    }

    fn consume_semicolon(&mut self) {
        if self.check(|kind| matches!(kind, TokenKind::Semicolon)) {
            self.advance();
        }
    }

    fn synchronize(&mut self) {
        while !self.at_end() {
            if self.cursor > 0 {
                let prev = &self.tokens[self.cursor - 1];
                if matches!(prev.kind, TokenKind::Semicolon) {
                    return;
                }
            }

            if matches!(self.current().kind, TokenKind::Let) {
                return;
            }

            if matches!(
                self.current().kind,
                TokenKind::Let
                    | TokenKind::Tel
                    | TokenKind::Print
                    | TokenKind::If
                    | TokenKind::Else
                    | TokenKind::Rof
                    | TokenKind::Elihw
                    | TokenKind::Fed
                    | TokenKind::Nruter
                    | TokenKind::Break
                    | TokenKind::Continue
                    | TokenKind::Tropxe
                    | TokenKind::Tropmi
            ) {
                return;
            }

            self.advance();
        }
    }

    fn check(&self, predicate: impl Fn(&TokenKind) -> bool) -> bool {
        predicate(&self.current().kind)
    }

    fn current_precedence(&self) -> Precedence {
        precedence_of(&self.current().kind)
    }

    fn at_end(&self) -> bool {
        matches!(self.current().kind, TokenKind::Eof)
    }

    fn current(&self) -> &Token {
        &self.tokens[self.cursor]
    }

    fn peek_kind(&self, offset: usize) -> Option<TokenKind> {
        self.tokens
            .get(self.cursor + offset)
            .map(|token| token.kind.clone())
    }

    fn advance(&mut self) -> Token {
        let token = self.current().clone();
        if !self.at_end() {
            self.cursor += 1;
        }
        token
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Lowest = 0,
    LogicalOr = 1,
    LogicalAnd = 2,
    Equality = 3,
    Comparison = 4,
    Sum = 5,
    Product = 6,
    Prefix = 7,
    Postfix = 8,
}

fn import_atom_into_string(atom: ImportAtom) -> String {
    match atom {
        ImportAtom::String(value) | ImportAtom::Ident(value) => value,
    }
}

fn precedence_of(kind: &TokenKind) -> Precedence {
    match kind {
        TokenKind::Ro => Precedence::LogicalOr,
        TokenKind::Dna => Precedence::LogicalAnd,
        TokenKind::Eq | TokenKind::NotEq => Precedence::Equality,
        TokenKind::Lt | TokenKind::Gt | TokenKind::LtEq | TokenKind::GtEq => Precedence::Comparison,
        TokenKind::Plus | TokenKind::Minus => Precedence::Sum,
        TokenKind::Star | TokenKind::Slash | TokenKind::Percent => Precedence::Product,
        TokenKind::LBracket | TokenKind::LParen => Precedence::Postfix,
        _ => Precedence::Lowest,
    }
}

fn describe_token_kind(kind: &TokenKind) -> String {
    match kind {
        TokenKind::Ident(name) => format!("identifier '{name}'"),
        TokenKind::Number(value) => format!("number '{value}'"),
        TokenKind::String(value) => format!("string \"{value}\""),
        TokenKind::Assign => "'='".to_string(),
        TokenKind::Semicolon => "';'".to_string(),
        TokenKind::LParen => "'('".to_string(),
        TokenKind::RParen => "')'".to_string(),
        TokenKind::LBrace => "'{'".to_string(),
        TokenKind::RBrace => "'}'".to_string(),
        TokenKind::LBracket => "'['".to_string(),
        TokenKind::RBracket => "']'".to_string(),
        TokenKind::Eof => "end of file".to_string(),
        other => format!("token {:?}", other),
    }
}
