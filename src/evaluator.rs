use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};

use crate::ast::{Expr, InfixOp, PrefixOp, Program, Stmt};
use crate::environment::Environment;
use crate::lexer::{LexOptions, tokenize_with_options};
use crate::parser::{Parser, ParserOptions};
use crate::preprocessor::preprocess_source;

mod inbuilt;

#[derive(Debug, Clone)]
pub enum Value {
    // Numeric values use f64, so very large results can overflow to `inf`
    // (for Fibonacci this happens starting at n = 1477).
    Number(f64),
    Boolean(bool),
    String(String),
    Array(Vec<Value>),
    Object(BTreeMap<String, Value>),
    Function(FunctionValue),
    Builtin(BuiltinFunction),
    Null,
}

#[derive(Debug, Clone)]
pub struct FunctionValue {
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<Stmt>,
    pub closure: Environment,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinFunction {
    Input,
    Len,
    ToNumber,
    Range,
    TypeOf,
    ToString,
    EmitTime,
    LamronTime,
    EtadToday,
    ReadFile,
    WriteFile,
    DeleteFile,
    HttpRequest,
}

impl BuiltinFunction {
    fn name(self) -> &'static str {
        match self {
            BuiltinFunction::Input => "input",
            BuiltinFunction::Len => "len",
            BuiltinFunction::ToNumber => "to_number",
            BuiltinFunction::Range => "range",
            BuiltinFunction::TypeOf => "type_of",
            BuiltinFunction::ToString => "to_string",
            BuiltinFunction::EmitTime => "__emit_time",
            BuiltinFunction::LamronTime => "__lamron_time",
            BuiltinFunction::EtadToday => "__etad_today",
            BuiltinFunction::ReadFile => "__read_file",
            BuiltinFunction::WriteFile => "__write_file",
            BuiltinFunction::DeleteFile => "__delete_file",
            BuiltinFunction::HttpRequest => "__http_request",
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Array(a), Value::Array(b)) => a == b,
            (Value::Object(a), Value::Object(b)) => a == b,
            (Value::Builtin(a), Value::Builtin(b)) => a == b,
            (Value::Null, Value::Null) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "{}", s),
            Value::Array(items) => {
                let rendered = items
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "[{}]", rendered)
            }
            Value::Object(entries) => {
                let rendered = entries
                    .iter()
                    .map(|(key, value)| format!("\"{}\": {}", key, value))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{{{}}}", rendered)
            }
            Value::Function(func) => write!(f, "<function {} / {}>", func.name, func.params.len()),
            Value::Builtin(BuiltinFunction::Input) => write!(f, "<builtin input>"),
            Value::Builtin(BuiltinFunction::Len) => write!(f, "<builtin len>"),
            Value::Builtin(BuiltinFunction::ToNumber) => write!(f, "<builtin to_number>"),
            Value::Builtin(BuiltinFunction::Range) => write!(f, "<builtin range>"),
            Value::Builtin(BuiltinFunction::TypeOf) => write!(f, "<builtin type_of>"),
            Value::Builtin(BuiltinFunction::ToString) => write!(f, "<builtin to_string>"),
            Value::Builtin(BuiltinFunction::EmitTime) => write!(f, "<builtin __emit_time>"),
            Value::Builtin(BuiltinFunction::LamronTime) => write!(f, "<builtin __lamron_time>"),
            Value::Builtin(BuiltinFunction::EtadToday) => write!(f, "<builtin __etad_today>"),
            Value::Builtin(BuiltinFunction::ReadFile) => write!(f, "<builtin __read_file>"),
            Value::Builtin(BuiltinFunction::WriteFile) => write!(f, "<builtin __write_file>"),
            Value::Builtin(BuiltinFunction::DeleteFile) => write!(f, "<builtin __delete_file>"),
            Value::Builtin(BuiltinFunction::HttpRequest) => write!(f, "<builtin __http_request>"),
            Value::Null => write!(f, "null"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EvalError {
    pub message: String,
    pub contexts: Vec<String>,
    pub hint: Option<String>,
}

impl EvalError {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            contexts: Vec::new(),
            hint: None,
        }
    }

    pub fn with_context(mut self, context: impl Into<String>) -> Self {
        self.contexts.push(context.into());
        self
    }

    pub fn with_hint(mut self, hint: impl Into<String>) -> Self {
        if self.hint.is_none() {
            self.hint = Some(hint.into());
        }
        self
    }
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "evaluation error: {}", self.message)?;
        for context in self.contexts.iter().rev() {
            write!(f, "\n  context: {context}")?;
        }
        if let Some(hint) = &self.hint {
            write!(f, "\n  hint: {hint}")?;
        }
        Ok(())
    }
}

pub struct Evaluator {
    env: Environment,
    input_buffer: VecDeque<String>,
    base_dir: PathBuf,
    module_cache: HashMap<PathBuf, HashMap<String, Value>>,
    import_stack: Vec<PathBuf>,
    call_depth: usize,
    options: EvaluatorOptions,
}

#[derive(Debug, Clone)]
enum EvalFlow {
    Value(Value),
    Return(Value),
    Break,
    Continue,
}

const MAX_LOOP_ITERATIONS: usize = 100_000;
const MAX_CALL_DEPTH: usize = 64;

#[derive(Debug, Clone, Copy)]
pub struct RuntimePermissions {
    pub allow_fs: bool,
    pub allow_net: bool,
}

impl RuntimePermissions {
    pub fn restricted() -> Self {
        Self {
            allow_fs: false,
            allow_net: false,
        }
    }
}

impl Default for RuntimePermissions {
    fn default() -> Self {
        Self::restricted()
    }
}

#[derive(Debug, Clone, Default)]
pub struct EvaluatorOptions {
    pub strict_tfel: bool,
    pub permissions: RuntimePermissions,
    pub module_search_paths: Vec<PathBuf>,
}

impl Evaluator {
    pub fn new() -> Self {
        Self::with_options(EvaluatorOptions::default())
    }

    pub fn with_options(options: EvaluatorOptions) -> Self {
        let env = Environment::new();
        inbuilt::install_builtins(&env);
        let base_dir = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
        Self {
            env,
            input_buffer: VecDeque::new(),
            base_dir,
            module_cache: HashMap::new(),
            import_stack: Vec::new(),
            call_depth: 0,
            options,
        }
    }

    pub fn with_base_dir(base_dir: impl Into<PathBuf>) -> Self {
        let mut evaluator = Self::with_options(EvaluatorOptions::default());
        evaluator.base_dir = base_dir.into();
        evaluator
    }

    pub fn with_base_dir_and_options(
        base_dir: impl Into<PathBuf>,
        options: EvaluatorOptions,
    ) -> Self {
        let mut evaluator = Self::with_options(options);
        evaluator.base_dir = base_dir.into();
        evaluator
    }

    pub fn eval_program(&mut self, program: &Program) -> Result<Value, EvalError> {
        let mut last = Value::Null;

        for (idx, stmt) in program.statements.iter().enumerate() {
            match self
                .eval_stmt(stmt)
                .map_err(|err| err.with_context(format!("at top-level statement {}", idx + 1)))?
            {
                EvalFlow::Value(value) => last = value,
                EvalFlow::Return(_) => {
                    return Err(EvalError::new(
                        "return statement used outside of a function",
                    ));
                }
                EvalFlow::Break => {
                    return Err(EvalError::new("break statement used outside of a loop"));
                }
                EvalFlow::Continue => {
                    return Err(EvalError::new("continue statement used outside of a loop"));
                }
            }
        }

        Ok(last)
    }

    fn eval_stmt(&mut self, stmt: &Stmt) -> Result<EvalFlow, EvalError> {
        match stmt {
            Stmt::Let { name, value } => {
                let evaluated = self.eval_expr(value)?;
                self.env.define(name.clone(), evaluated.clone());
                Ok(EvalFlow::Value(evaluated))
            }
            // TFEL assignment updates an existing binding through enclosing scopes.
            // If missing, it defines the binding in the current scope.
            Stmt::Assign { name, value } => {
                let evaluated = self.eval_expr(value)?;
                if !self.env.assign(name, &evaluated) {
                    if self.options.strict_tfel {
                        return Err(EvalError::new(format!(
                            "assignment to undefined variable '{}' in --strict-tfel mode",
                            name
                        ))
                        .with_hint(
                            "declare variables explicitly first, e.g. `tel name = value;`",
                        ));
                    }
                    self.env.define(name.clone(), evaluated.clone());
                }
                Ok(EvalFlow::Value(evaluated))
            }
            Stmt::Print { value } => {
                // Normal equivalent: print(value).
                let evaluated = self.eval_expr(value)?;
                println!("{}", evaluated);
                Ok(EvalFlow::Value(evaluated))
            }
            Stmt::Expr(expr) => Ok(EvalFlow::Value(self.eval_expr(expr)?)),
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition = self.eval_expr(condition)?;
                if is_truthy(&condition) {
                    self.eval_block_scoped(then_branch)
                } else if let Some(else_branch) = else_branch {
                    self.eval_block_scoped(else_branch)
                } else {
                    Ok(EvalFlow::Value(Value::Null))
                }
            }
            Stmt::While { condition, body } => self.eval_while(condition, body),
            Stmt::For {
                name,
                iterable,
                body,
            } => self.eval_for(name, iterable, body),
            Stmt::FunctionDef { name, params, body } => {
                let function = Value::Function(FunctionValue {
                    name: name.clone(),
                    params: params.clone(),
                    body: body.clone(),
                    closure: self.env.clone(),
                });
                self.env.define(name.clone(), function.clone());
                Ok(EvalFlow::Value(function))
            }
            Stmt::Return(value) => {
                let value = if let Some(value) = value {
                    self.eval_expr(value)?
                } else {
                    Value::Null
                };
                Ok(EvalFlow::Return(value))
            }
            Stmt::Break => Ok(EvalFlow::Break),
            Stmt::Continue => Ok(EvalFlow::Continue),
            Stmt::Export { .. } => Ok(EvalFlow::Value(Value::Null)),
            Stmt::Import { module, item } => {
                self.eval_import(module, item.as_deref())?;
                Ok(EvalFlow::Value(Value::Null))
            }
        }
    }

    fn eval_import(&mut self, module_name: &str, item: Option<&str>) -> Result<(), EvalError> {
        let module_path = self
            .resolve_module_path(module_name)
            .map_err(|err| err.with_context(format!("while importing module '{module_name}'")))?;
        let exports = self
            .load_module_exports(&module_path)
            .map_err(|err| err.with_context(format!("while importing module '{module_name}'")))?;
        let namespace = module_namespace(module_name);

        if let Some(item_name) = item {
            let value = exports.get(item_name).cloned().ok_or_else(|| {
                let mut error = EvalError::new(format!(
                    "module '{}' does not export '{}'",
                    module_name, item_name
                ));
                if let Some(suggestion) =
                    closest_name(item_name, &exports.keys().cloned().collect::<Vec<_>>())
                {
                    error = error.with_hint(format!("did you mean '{}'?", suggestion));
                }
                error.with_context(format!("while importing module '{module_name}'"))
            })?;
            self.env.define(item_name.to_string(), value);
            return Ok(());
        }

        for (name, value) in exports {
            self.env.define(name.clone(), value.clone());
            if let Some(namespace) = &namespace {
                self.env.define(format!("{namespace}.{name}"), value);
            }
        }

        Ok(())
    }

    fn resolve_module_path(&self, module_name: &str) -> Result<PathBuf, EvalError> {
        let raw = PathBuf::from(module_name);
        let candidate_paths = if raw.extension().is_some() {
            vec![raw]
        } else {
            vec![
                raw.with_extension("tfel"),
                raw.join("mod.tfel"),
                raw.join("index.tfel"),
            ]
        };

        let mut roots = vec![self.base_dir.clone()];
        roots.extend(self.options.module_search_paths.iter().cloned());

        let mut searched = Vec::<PathBuf>::new();
        let mut seen = HashSet::<PathBuf>::new();

        for candidate in candidate_paths {
            if candidate.is_absolute() {
                if seen.insert(candidate.clone()) {
                    searched.push(candidate.clone());
                }
                if candidate.exists() {
                    return Ok(fs::canonicalize(&candidate).unwrap_or(candidate));
                }
                continue;
            }

            for root in &roots {
                let resolved = root.join(&candidate);
                if seen.insert(resolved.clone()) {
                    searched.push(resolved.clone());
                }
                if resolved.exists() {
                    return Ok(fs::canonicalize(&resolved).unwrap_or(resolved));
                }
            }
        }

        let mut error = EvalError::new(format!("module '{module_name}' was not found"));
        let searched_lines = searched
            .iter()
            .map(|path| format!("  - {}", path.display()))
            .collect::<Vec<_>>();
        if !searched_lines.is_empty() {
            error = error.with_hint(format!("searched paths:\n{}", searched_lines.join("\n")));
        }
        Err(error)
    }

    fn load_module_exports(
        &mut self,
        module_path: &Path,
    ) -> Result<HashMap<String, Value>, EvalError> {
        if let Some(cached) = self.module_cache.get(module_path) {
            return Ok(cached.clone());
        }

        let normalized_path = module_path.to_path_buf();
        if self.import_stack.contains(&normalized_path) {
            let mut chain = self
                .import_stack
                .iter()
                .map(|p| p.display().to_string())
                .collect::<Vec<_>>();
            chain.push(normalized_path.display().to_string());
            return Err(EvalError::new(format!(
                "cyclic import detected: {}",
                chain.join(" -> ")
            )));
        }

        let source = fs::read_to_string(&normalized_path).map_err(|err| {
            EvalError::new(format!(
                "failed to read module '{}': {}",
                normalized_path.display(),
                err
            ))
        })?;

        let preprocessed = preprocess_source(&source);
        let tokens = tokenize_with_options(
            &preprocessed,
            LexOptions {
                strict_tfel: self.options.strict_tfel,
            },
        )
        .map_err(|errors| {
            let first = &errors[0];
            let extra = errors.len().saturating_sub(1);
            if extra == 0 {
                EvalError::new(format!(
                    "failed to lex module '{}': {} at {}..{}",
                    normalized_path.display(),
                    first.message,
                    first.span.start,
                    first.span.end
                ))
            } else {
                EvalError::new(format!(
                    "failed to lex module '{}': {} at {}..{} (+{} more)",
                    normalized_path.display(),
                    first.message,
                    first.span.start,
                    first.span.end,
                    extra
                ))
            }
        })?;

        let program = Parser::with_options(
            tokens,
            ParserOptions {
                strict_tfel: self.options.strict_tfel,
            },
        )
        .parse_program()
        .map_err(|errors| {
            let first = &errors[0];
            let extra = errors.len().saturating_sub(1);
            if extra == 0 {
                EvalError::new(format!(
                    "failed to parse module '{}': {} at {}..{}",
                    normalized_path.display(),
                    first.message,
                    first.span.start,
                    first.span.end
                ))
            } else {
                EvalError::new(format!(
                    "failed to parse module '{}': {} at {}..{} (+{} more)",
                    normalized_path.display(),
                    first.message,
                    first.span.start,
                    first.span.end,
                    extra
                ))
            }
        })?;
        let explicit_exports = collect_explicit_exports(&program);

        self.import_stack.push(normalized_path.clone());

        let previous_env = self.env.clone();
        let previous_base_dir = self.base_dir.clone();

        let module_env = Environment::new();
        inbuilt::install_builtins(&module_env);
        let builtin_names = module_env
            .snapshot_current_scope()
            .into_keys()
            .collect::<HashSet<_>>();

        self.env = module_env;
        self.base_dir = normalized_path
            .parent()
            .map(Path::to_path_buf)
            .unwrap_or_else(|| previous_base_dir.clone());

        let module_eval = self.eval_program(&program).map_err(|err| {
            err.with_context(format!(
                "while evaluating module '{}'",
                normalized_path.display()
            ))
        });
        let exports_result = module_eval.and_then(|_| {
            let current_scope = self.env.snapshot_current_scope();
            if let Some(explicit) = &explicit_exports {
                let mut exports = HashMap::new();
                for name in explicit {
                    if builtin_names.contains(name) {
                        continue;
                    }
                    let value = current_scope.get(name).cloned().ok_or_else(|| {
                        EvalError::new(format!(
                            "module '{}' exports '{}' but it is not defined",
                            normalized_path.display(),
                            name
                        ))
                    })?;
                    exports.insert(name.clone(), value);
                }
                Ok(exports)
            } else {
                Ok(current_scope
                    .into_iter()
                    .filter(|(name, _)| !builtin_names.contains(name))
                    .collect::<HashMap<_, _>>())
            }
        });

        self.env = previous_env;
        self.base_dir = previous_base_dir;
        self.import_stack.pop();

        let exports = exports_result?;
        self.module_cache
            .insert(normalized_path.clone(), exports.clone());

        Ok(exports)
    }

    fn eval_expr(&mut self, expr: &Expr) -> Result<Value, EvalError> {
        match expr {
            Expr::Identifier(name) => self
                .env
                .get(name)
                .ok_or_else(|| unknown_variable_error(&self.env, name)),
            Expr::Number(value) => Ok(Value::Number(*value)),
            Expr::String(value) => self.eval_string_literal(value),
            Expr::Boolean(value) => Ok(Value::Boolean(*value)),
            Expr::Array(items) => {
                let mut evaluated = Vec::with_capacity(items.len());
                for item in items {
                    evaluated.push(self.eval_expr(item)?);
                }
                Ok(Value::Array(evaluated))
            }
            Expr::Object(entries) => {
                let mut evaluated = BTreeMap::new();
                for (key, value_expr) in entries {
                    evaluated.insert(key.clone(), self.eval_expr(value_expr)?);
                }
                Ok(Value::Object(evaluated))
            }
            Expr::Index { target, index } => {
                let target = self.eval_expr(target)?;
                let index = self.eval_expr(index)?;
                match (target, index) {
                    (Value::Array(items), Value::Number(i)) => {
                        let idx = inbuilt::normalize_index(i, items.len(), "array")?;
                        items
                            .get(idx)
                            .cloned()
                            .ok_or_else(|| EvalError::new("array index out of bounds"))
                    }
                    (Value::String(text), Value::Number(i)) => {
                        let chars = text.chars().collect::<Vec<_>>();
                        let idx = inbuilt::normalize_index(i, chars.len(), "string")?;
                        chars
                            .get(idx)
                            .map(|ch| Value::String(ch.to_string()))
                            .ok_or_else(|| EvalError::new("string index out of bounds"))
                    }
                    (Value::Object(entries), Value::String(key)) => entries
                        .get(&key)
                        .cloned()
                        .ok_or_else(|| EvalError::new(format!("object key '{}' not found", key))),
                    (Value::Object(_), other) => Err(EvalError::new(format!(
                        "object key must be a string, got '{}'",
                        other
                    ))),
                    (_, Value::Number(_)) => Err(EvalError::new(
                        "indexing currently supports arrays, strings, and objects",
                    )),
                    (_, other) => Err(EvalError::new(format!(
                        "index value must be a number )arrays/strings( or string )objects(, got '{}'",
                        other
                    ))),
                }
            }
            Expr::Call { callee, args } => {
                let callee = self.eval_expr(callee)?;
                let mut values = Vec::with_capacity(args.len());
                for arg in args {
                    values.push(self.eval_expr(arg)?);
                }
                self.eval_call(callee, values)
            }
            Expr::Prefix { op, rhs } => {
                let rhs = self.eval_expr(rhs)?;
                self.eval_prefix(*op, rhs)
            }
            Expr::Infix { lhs, op, rhs } => {
                if matches!(op, InfixOp::And) {
                    let lhs = self.eval_expr(lhs)?;
                    if !is_truthy(&lhs) {
                        return Ok(Value::Boolean(false));
                    }
                    let rhs = self.eval_expr(rhs)?;
                    return Ok(Value::Boolean(is_truthy(&rhs)));
                }

                if matches!(op, InfixOp::Or) {
                    let lhs = self.eval_expr(lhs)?;
                    if is_truthy(&lhs) {
                        return Ok(Value::Boolean(true));
                    }
                    let rhs = self.eval_expr(rhs)?;
                    return Ok(Value::Boolean(is_truthy(&rhs)));
                }

                let lhs = self.eval_expr(lhs)?;
                let rhs = self.eval_expr(rhs)?;
                self.eval_infix(lhs, *op, rhs)
            }
        }
    }

    fn eval_string_literal(&mut self, value: &str) -> Result<Value, EvalError> {
        if !value.contains("${") {
            return Ok(Value::String(value.to_string()));
        }
        self.interpolate_string(value).map(Value::String)
    }

    fn interpolate_string(&mut self, template: &str) -> Result<String, EvalError> {
        let mut out = String::new();
        let mut cursor = 0usize;

        while let Some(found) = template[cursor..].find("${") {
            let open = cursor + found;
            out.push_str(&template[cursor..open]);
            let expr_start = open + 2;
            let expr_end = find_interpolation_end(template, expr_start).ok_or_else(|| {
                EvalError::new("unterminated string interpolation (missing '}')")
                    .with_hint("close interpolation segments as `${expr}`")
            })?;
            let expression = template[expr_start..expr_end].trim();
            if expression.is_empty() {
                return Err(EvalError::new(
                    "empty interpolation expression `${}` is not allowed",
                ));
            }

            let value = self
                .eval_interpolation_expression(expression)
                .map_err(|err| err.with_context("while evaluating string interpolation"))?;
            out.push_str(&value.to_string());
            cursor = expr_end + 1;
        }

        out.push_str(&template[cursor..]);
        Ok(out)
    }

    fn eval_interpolation_expression(&mut self, expression: &str) -> Result<Value, EvalError> {
        let source = format!("{expression};");
        let tokens = tokenize_with_options(
            &source,
            LexOptions {
                strict_tfel: self.options.strict_tfel,
            },
        )
        .map_err(|errors| {
            let first = &errors[0];
            EvalError::new(format!(
                "invalid interpolation expression '{}': {} at {}..{}",
                expression, first.message, first.span.start, first.span.end
            ))
        })?;
        let program = Parser::with_options(
            tokens,
            ParserOptions {
                strict_tfel: self.options.strict_tfel,
            },
        )
        .parse_program()
        .map_err(|errors| {
            let first = &errors[0];
            EvalError::new(format!(
                "invalid interpolation expression '{}': {} at {}..{}",
                expression, first.message, first.span.start, first.span.end
            ))
        })?;

        if program.statements.len() != 1 {
            return Err(EvalError::new(
                "interpolation must contain exactly one expression",
            ));
        }

        match &program.statements[0] {
            Stmt::Expr(expr) => self.eval_expr(expr),
            _ => Err(EvalError::new(
                "interpolation must be an expression, not a statement",
            )),
        }
    }

    fn eval_block(&mut self, block: &[Stmt]) -> Result<EvalFlow, EvalError> {
        let mut last = Value::Null;
        for (idx, stmt) in block.iter().enumerate() {
            match self
                .eval_stmt(stmt)
                .map_err(|err| err.with_context(format!("inside block statement {}", idx + 1)))?
            {
                EvalFlow::Value(value) => last = value,
                EvalFlow::Return(value) => return Ok(EvalFlow::Return(value)),
                EvalFlow::Break => return Ok(EvalFlow::Break),
                EvalFlow::Continue => return Ok(EvalFlow::Continue),
            }
        }
        Ok(EvalFlow::Value(last))
    }

    fn eval_block_scoped(&mut self, block: &[Stmt]) -> Result<EvalFlow, EvalError> {
        let parent = self.env.clone();
        self.env = Environment::new_enclosed(parent.clone());
        let result = self.eval_block(block);
        self.env = parent;
        result
    }

    fn eval_call(&mut self, callee: Value, args: Vec<Value>) -> Result<Value, EvalError> {
        match callee {
            Value::Function(function) => {
                if function.params.len() != args.len() {
                    return Err(EvalError::new(format!(
                        "function '{}' expected {} argument(s), got {}",
                        function.name,
                        function.params.len(),
                        args.len()
                    )));
                }

                if self.call_depth >= MAX_CALL_DEPTH {
                    return Err(EvalError::new(format!(
                        "function call depth exceeded limit ({MAX_CALL_DEPTH})"
                    ))
                    .with_hint("rewrite deep recursion as an iterative loop"));
                }

                self.call_depth += 1;
                let outer_env = self.env.clone();
                let call_env = Environment::new_enclosed(function.closure.clone());
                self.env = call_env;

                for (param, arg) in function.params.iter().zip(args.into_iter()) {
                    self.env.define(param.clone(), arg);
                }

                let result = self.eval_block(&function.body).map_err(|err| {
                    err.with_context(format!("while calling function '{}'", function.name))
                });
                self.env = outer_env;
                self.call_depth -= 1;

                match result? {
                    EvalFlow::Value(value) => Ok(value),
                    EvalFlow::Return(value) => Ok(value),
                    EvalFlow::Break => {
                        Err(EvalError::new("break statement used outside of a loop"))
                    }
                    EvalFlow::Continue => {
                        Err(EvalError::new("continue statement used outside of a loop"))
                    }
                }
            }
            Value::Builtin(builtin) => self.eval_builtin_call(builtin, args).map_err(|err| {
                err.with_context(format!("while calling builtin '{}'", builtin.name()))
            }),
            _ => Err(EvalError::new("attempted to call a non-function value")),
        }
    }

    fn eval_while(&mut self, condition: &Expr, body: &[Stmt]) -> Result<EvalFlow, EvalError> {
        let mut iterations = 0usize;
        let mut last = Value::Null;

        loop {
            if iterations >= MAX_LOOP_ITERATIONS {
                return Err(EvalError::new(format!(
                    "while loop exceeded iteration limit ({MAX_LOOP_ITERATIONS})"
                )));
            }
            iterations += 1;

            let condition_value = self.eval_expr(condition)?;
            if !is_truthy(&condition_value) {
                break;
            }

            match self.eval_block_scoped(body)? {
                EvalFlow::Value(value) => last = value,
                EvalFlow::Return(value) => return Ok(EvalFlow::Return(value)),
                EvalFlow::Break => break,
                EvalFlow::Continue => continue,
            }
        }

        Ok(EvalFlow::Value(last))
    }

    fn eval_for(
        &mut self,
        name: &str,
        iterable: &Expr,
        body: &[Stmt],
    ) -> Result<EvalFlow, EvalError> {
        let iterable_value = self.eval_expr(iterable)?;
        let elements = inbuilt::iterable_to_values(iterable_value)?;

        let outer_env = self.env.clone();
        self.env = Environment::new_enclosed(outer_env.clone());

        let mut last = Value::Null;
        for element in elements {
            self.env.define(name.to_string(), element);
            match self.eval_block_scoped(body)? {
                EvalFlow::Value(value) => last = value,
                EvalFlow::Return(value) => {
                    self.env = outer_env;
                    return Ok(EvalFlow::Return(value));
                }
                EvalFlow::Break => {
                    self.env = outer_env;
                    return Ok(EvalFlow::Value(last));
                }
                EvalFlow::Continue => continue,
            }
        }

        self.env = outer_env;
        Ok(EvalFlow::Value(last))
    }

    fn eval_prefix(&self, op: PrefixOp, rhs: Value) -> Result<Value, EvalError> {
        match op {
            PrefixOp::Not => Ok(Value::Boolean(!is_truthy(&rhs))),
            PrefixOp::Negate => match rhs {
                Value::Number(n) => Ok(Value::Number(-n)),
                other => Err(EvalError::new(format!("cannot negate value '{}'", other))),
            },
        }
    }

    fn eval_infix(&self, lhs: Value, op: InfixOp, rhs: Value) -> Result<Value, EvalError> {
        match op {
            InfixOp::And => Ok(Value::Boolean(is_truthy(&lhs) && is_truthy(&rhs))),
            InfixOp::Or => Ok(Value::Boolean(is_truthy(&lhs) || is_truthy(&rhs))),
            InfixOp::Add => match (lhs, rhs) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
                (Value::String(a), Value::String(b)) => Ok(Value::String(format!("{}{}", a, b))),
                (a, b) => Err(EvalError::new(format!("cannot add '{}' and '{}'", a, b))),
            },
            InfixOp::Subtract => numbers(lhs, rhs, |a, b| Value::Number(a - b), "subtract"),
            InfixOp::Multiply => numbers(lhs, rhs, |a, b| Value::Number(a * b), "multiply"),
            InfixOp::Divide => match (lhs, rhs) {
                (Value::Number(_), Value::Number(0.0)) => {
                    Err(EvalError::new("division by zero is not allowed"))
                }
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a / b)),
                (a, b) => Err(EvalError::new(format!("cannot divide '{}' and '{}'", a, b))),
            },
            InfixOp::Modulo => match (lhs, rhs) {
                (Value::Number(_), Value::Number(0.0)) => {
                    Err(EvalError::new("modulo by zero is not allowed"))
                }
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a % b)),
                (a, b) => Err(EvalError::new(format!(
                    "cannot apply modulo to '{}' and '{}'",
                    a, b
                ))),
            },
            InfixOp::Eq => Ok(Value::Boolean(lhs == rhs)),
            InfixOp::NotEq => Ok(Value::Boolean(lhs != rhs)),
            InfixOp::Lt => numbers(lhs, rhs, |a, b| Value::Boolean(a < b), "compare"),
            InfixOp::Gt => numbers(lhs, rhs, |a, b| Value::Boolean(a > b), "compare"),
            InfixOp::LtEq => numbers(lhs, rhs, |a, b| Value::Boolean(a <= b), "compare"),
            InfixOp::GtEq => numbers(lhs, rhs, |a, b| Value::Boolean(a >= b), "compare"),
        }
    }
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}

fn collect_explicit_exports(program: &Program) -> Option<HashSet<String>> {
    let mut names = HashSet::new();
    let mut saw_export_stmt = false;

    for stmt in &program.statements {
        if let Stmt::Export {
            names: export_names,
        } = stmt
        {
            saw_export_stmt = true;
            for name in export_names {
                names.insert(name.clone());
            }
        }
    }

    if saw_export_stmt { Some(names) } else { None }
}

fn module_namespace(module_name: &str) -> Option<String> {
    let path = Path::new(module_name);
    let stem = path.file_stem().and_then(|part| part.to_str())?;
    if stem == "mod" || stem == "index" {
        path.parent()
            .and_then(Path::file_name)
            .and_then(|part| part.to_str())
            .filter(|part| !part.is_empty())
            .map(str::to_string)
    } else {
        Some(stem.to_string())
    }
}

fn unknown_variable_error(env: &Environment, name: &str) -> EvalError {
    let mut error = EvalError::new(format!("unknown variable '{name}'"));
    let names = env.visible_names();
    if let Some(suggestion) = closest_name(name, &names) {
        error = error.with_hint(format!("did you mean '{}'?", suggestion));
    }
    error
}

fn closest_name(name: &str, candidates: &[String]) -> Option<String> {
    let mut best: Option<(&String, usize)> = None;
    for candidate in candidates {
        let distance = edit_distance(name, candidate);
        match best {
            Some((_, best_distance)) if distance >= best_distance => {}
            _ => best = Some((candidate, distance)),
        }
    }

    let (candidate, distance) = best?;
    let threshold = (name.chars().count() / 3).max(2);
    if distance <= threshold {
        Some(candidate.clone())
    } else {
        None
    }
}

fn edit_distance(a: &str, b: &str) -> usize {
    let a = a.chars().collect::<Vec<_>>();
    let b = b.chars().collect::<Vec<_>>();

    if a.is_empty() {
        return b.len();
    }
    if b.is_empty() {
        return a.len();
    }

    let mut prev = (0..=b.len()).collect::<Vec<_>>();
    let mut curr = vec![0; b.len() + 1];

    for (i, ca) in a.iter().enumerate() {
        curr[0] = i + 1;
        for (j, cb) in b.iter().enumerate() {
            let cost = usize::from(ca != cb);
            curr[j + 1] = (prev[j + 1] + 1).min(curr[j] + 1).min(prev[j] + cost);
        }
        prev.copy_from_slice(&curr);
    }

    prev[b.len()]
}

fn numbers(
    lhs: Value,
    rhs: Value,
    op: impl FnOnce(f64, f64) -> Value,
    label: &'static str,
) -> Result<Value, EvalError> {
    match (lhs, rhs) {
        (Value::Number(a), Value::Number(b)) => Ok(op(a, b)),
        (a, b) => Err(EvalError::new(format!(
            "cannot {} '{}' and '{}'",
            label, a, b
        ))),
    }
}

fn is_truthy(value: &Value) -> bool {
    match value {
        Value::Boolean(b) => *b,
        Value::Null => false,
        Value::Number(n) => *n != 0.0,
        Value::String(s) => !s.is_empty(),
        Value::Array(items) => !items.is_empty(),
        Value::Object(entries) => !entries.is_empty(),
        Value::Function(_) => true,
        Value::Builtin(_) => true,
    }
}

fn find_interpolation_end(template: &str, start: usize) -> Option<usize> {
    let mut depth = 0usize;
    let mut in_string = false;
    let mut escaped = false;

    for (offset, ch) in template[start..].char_indices() {
        let idx = start + offset;

        if in_string {
            if escaped {
                escaped = false;
                continue;
            }
            if ch == '\\' {
                escaped = true;
                continue;
            }
            if ch == '"' {
                in_string = false;
            }
            continue;
        }

        if ch == '"' {
            in_string = true;
            continue;
        }

        if ch == '{' {
            depth += 1;
            continue;
        }

        if ch == '}' {
            if depth == 0 {
                return Some(idx);
            }
            depth -= 1;
        }
    }

    None
}
