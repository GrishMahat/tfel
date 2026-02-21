use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt;
use std::fs;
use std::io::{self, Read, Write};
use std::net::{TcpStream, ToSocketAddrs};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::process::Stdio;
use std::time::Duration;

use crate::ast::{Expr, InfixOp, PrefixOp, Program, Stmt};
use crate::environment::Environment;
use crate::lexer::tokenize;
use crate::parser::Parser;
use crate::preprocessor::preprocess_source;

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    String(String),
    Array(Vec<Value>),
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
}

#[derive(Debug, Clone)]
enum EvalFlow {
    Value(Value),
    Return(Value),
}

const MAX_LOOP_ITERATIONS: usize = 100_000;
const MAX_CALL_DEPTH: usize = 64;

impl Evaluator {
    pub fn new() -> Self {
        let env = Environment::new();
        install_builtins(&env);
        let base_dir = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
        Self {
            env,
            input_buffer: VecDeque::new(),
            base_dir,
            module_cache: HashMap::new(),
            import_stack: Vec::new(),
            call_depth: 0,
        }
    }

    pub fn with_base_dir(base_dir: impl Into<PathBuf>) -> Self {
        let mut evaluator = Self::new();
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
            Stmt::Import { module, item } => {
                self.eval_import(module, item.as_deref())?;
                Ok(EvalFlow::Value(Value::Null))
            }
        }
    }

    fn eval_import(&mut self, module_name: &str, item: Option<&str>) -> Result<(), EvalError> {
        let module_path = self.resolve_module_path(module_name);
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

    fn resolve_module_path(&self, module_name: &str) -> PathBuf {
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

        for candidate in candidate_paths {
            let resolved = if candidate.is_absolute() {
                candidate
            } else {
                self.base_dir.join(candidate)
            };

            if resolved.exists() {
                return fs::canonicalize(&resolved).unwrap_or(resolved);
            }
        }

        let fallback = if module_name.ends_with(".tfel") {
            PathBuf::from(module_name)
        } else {
            PathBuf::from(module_name).with_extension("tfel")
        };
        let resolved = if fallback.is_absolute() {
            fallback
        } else {
            self.base_dir.join(fallback)
        };
        fs::canonicalize(&resolved).unwrap_or(resolved)
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
        let tokens = tokenize(&preprocessed).map_err(|errors| {
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

        let program = Parser::new(tokens).parse_program().map_err(|errors| {
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

        self.import_stack.push(normalized_path.clone());

        let previous_env = self.env.clone();
        let previous_base_dir = self.base_dir.clone();

        let module_env = Environment::new();
        install_builtins(&module_env);
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
        let exports_result = module_eval.map(|_| {
            self.env
                .snapshot_current_scope()
                .into_iter()
                .filter(|(name, _)| !builtin_names.contains(name))
                .collect::<HashMap<_, _>>()
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
            Expr::String(value) => Ok(Value::String(value.clone())),
            Expr::Boolean(value) => Ok(Value::Boolean(*value)),
            Expr::Array(items) => {
                let mut evaluated = Vec::with_capacity(items.len());
                for item in items {
                    evaluated.push(self.eval_expr(item)?);
                }
                Ok(Value::Array(evaluated))
            }
            Expr::Index { target, index } => {
                let target = self.eval_expr(target)?;
                let index = self.eval_expr(index)?;
                match (target, index) {
                    (Value::Array(items), Value::Number(i)) => {
                        let idx = normalize_index(i, items.len(), "array")?;
                        items
                            .get(idx)
                            .cloned()
                            .ok_or_else(|| EvalError::new("array index out of bounds"))
                    }
                    (Value::String(text), Value::Number(i)) => {
                        let chars = text.chars().collect::<Vec<_>>();
                        let idx = normalize_index(i, chars.len(), "string")?;
                        chars
                            .get(idx)
                            .map(|ch| Value::String(ch.to_string()))
                            .ok_or_else(|| EvalError::new("string index out of bounds"))
                    }
                    (_, Value::Number(_)) => Err(EvalError::new(
                        "indexing currently supports arrays and strings only",
                    )),
                    (_, other) => Err(EvalError::new(format!(
                        "index value must be a number, got '{}'",
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

    fn eval_block(&mut self, block: &[Stmt]) -> Result<EvalFlow, EvalError> {
        let mut last = Value::Null;
        for (idx, stmt) in block.iter().enumerate() {
            match self
                .eval_stmt(stmt)
                .map_err(|err| err.with_context(format!("inside block statement {}", idx + 1)))?
            {
                EvalFlow::Value(value) => last = value,
                EvalFlow::Return(value) => return Ok(EvalFlow::Return(value)),
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
                    return Err(
                        EvalError::new(format!(
                            "function call depth exceeded limit ({MAX_CALL_DEPTH})"
                        ))
                        .with_hint("rewrite deep recursion as an iterative loop"),
                    );
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
        let elements = iterable_to_values(iterable_value)?;

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
            }
        }

        self.env = outer_env;
        Ok(EvalFlow::Value(last))
    }

    fn eval_builtin_call(
        &mut self,
        builtin: BuiltinFunction,
        args: Vec<Value>,
    ) -> Result<Value, EvalError> {
        match builtin {
            BuiltinFunction::Input => self.eval_builtin_input(args),
            BuiltinFunction::Len => eval_builtin_len(args),
            BuiltinFunction::ToNumber => eval_builtin_to_number(args),
            BuiltinFunction::Range => eval_builtin_range(args),
            BuiltinFunction::TypeOf => eval_builtin_type_of(args),
            BuiltinFunction::ToString => eval_builtin_to_string(args),
            BuiltinFunction::EmitTime => self.eval_builtin_emit_time(args),
            BuiltinFunction::LamronTime => self.eval_builtin_lamron_time(args),
            BuiltinFunction::EtadToday => self.eval_builtin_etad_today(args),
            BuiltinFunction::ReadFile => self.eval_builtin_read_file(args),
            BuiltinFunction::WriteFile => self.eval_builtin_write_file(args),
            BuiltinFunction::DeleteFile => self.eval_builtin_delete_file(args),
            BuiltinFunction::HttpRequest => self.eval_builtin_http_request(args),
        }
    }

    fn eval_builtin_input(&mut self, args: Vec<Value>) -> Result<Value, EvalError> {
        if args.len() > 1 {
            return Err(EvalError::new(format!(
                "input expected 0 or 1 argument(s), got {}",
                args.len()
            )));
        }

        if let Some(prompt) = args.first() {
            print!("{}", prompt);
            io::stdout()
                .flush()
                .map_err(|err| EvalError::new(format!("failed to flush stdout: {err}")))?;
        }

        let line = self.read_input_line()?;
        let trimmed = line.trim_end_matches(['\n', '\r']).to_string();
        Ok(Value::String(trimmed))
    }

    fn read_input_line(&mut self) -> Result<String, EvalError> {
        if let Some(line) = self.input_buffer.pop_front() {
            return Ok(line);
        }

        let mut line = String::new();
        io::stdin()
            .read_line(&mut line)
            .map_err(|err| EvalError::new(format!("failed to read input: {err}")))?;
        Ok(line)
    }

    fn eval_builtin_emit_time(&mut self, args: Vec<Value>) -> Result<Value, EvalError> {
        if args.len() > 1 {
            return Err(EvalError::new(format!(
                "__emit_time expected 0 or 1 argument(s), got {}",
                args.len()
            )));
        }

        let raw = if let Some(value) = args.first() {
            expect_string_arg(value, "__emit_time", 1)?
        } else {
            self.read_system_clock("%H:%M")?
        };
        let swapped = swap_hhmm(&raw)?;
        Ok(Value::String(swapped))
    }

    fn eval_builtin_lamron_time(&mut self, args: Vec<Value>) -> Result<Value, EvalError> {
        if !args.is_empty() {
            return Err(EvalError::new(format!(
                "__lamron_time expected 0 argument(s), got {}",
                args.len()
            )));
        }
        Ok(Value::String(self.read_system_clock("%H:%M")?))
    }

    fn eval_builtin_etad_today(&mut self, args: Vec<Value>) -> Result<Value, EvalError> {
        if !args.is_empty() {
            return Err(EvalError::new(format!(
                "__etad_today expected 0 argument(s), got {}",
                args.len()
            )));
        }
        Ok(Value::String(self.read_system_clock("%Y-%m-%d")?))
    }

    fn eval_builtin_read_file(&mut self, args: Vec<Value>) -> Result<Value, EvalError> {
        if args.len() != 1 {
            return Err(EvalError::new(format!(
                "__read_file expected 1 argument(s), got {}",
                args.len()
            )));
        }

        let path = expect_string_arg(&args[0], "__read_file", 1)?;
        let resolved = self.resolve_runtime_path(&path);
        let contents = fs::read_to_string(&resolved).map_err(|err| {
            EvalError::new(format!(
                "failed to read file '{}': {}",
                resolved.display(),
                err
            ))
        })?;
        Ok(Value::String(contents))
    }

    fn eval_builtin_write_file(&mut self, args: Vec<Value>) -> Result<Value, EvalError> {
        if args.len() != 2 {
            return Err(EvalError::new(format!(
                "__write_file expected 2 argument(s), got {}",
                args.len()
            )));
        }

        let path = expect_string_arg(&args[0], "__write_file", 1)?;
        let contents = args[1].to_string();
        let resolved = self.resolve_runtime_path(&path);

        if let Some(parent) = resolved.parent() {
            fs::create_dir_all(parent).map_err(|err| {
                EvalError::new(format!(
                    "failed to create directory '{}': {}",
                    parent.display(),
                    err
                ))
            })?;
        }

        fs::write(&resolved, &contents).map_err(|err| {
            EvalError::new(format!(
                "failed to write file '{}': {}",
                resolved.display(),
                err
            ))
        })?;

        Ok(Value::Number(contents.len() as f64))
    }

    fn eval_builtin_delete_file(&mut self, args: Vec<Value>) -> Result<Value, EvalError> {
        if args.len() != 1 {
            return Err(EvalError::new(format!(
                "__delete_file expected 1 argument(s), got {}",
                args.len()
            )));
        }

        let path = expect_string_arg(&args[0], "__delete_file", 1)?;
        let resolved = self.resolve_runtime_path(&path);
        match fs::remove_file(&resolved) {
            Ok(()) => Ok(Value::Boolean(true)),
            Err(err) if err.kind() == io::ErrorKind::NotFound => Ok(Value::Boolean(false)),
            Err(err) => Err(EvalError::new(format!(
                "failed to delete file '{}': {}",
                resolved.display(),
                err
            ))),
        }
    }

    fn eval_builtin_http_request(&mut self, args: Vec<Value>) -> Result<Value, EvalError> {
        if args.len() < 2 || args.len() > 3 {
            return Err(EvalError::new(format!(
                "__http_request expected 2 or 3 argument(s), got {}",
                args.len()
            )));
        }

        let method = expect_string_arg(&args[0], "__http_request", 1)?;
        let url = expect_string_arg(&args[1], "__http_request", 2)?;
        let body = if args.len() == 3 {
            expect_string_arg(&args[2], "__http_request", 3)?
        } else {
            String::new()
        };

        let response = send_http_request(&method, &url, &body)?;
        Ok(Value::Array(vec![
            Value::Number(response.status as f64),
            Value::String(response.body),
        ]))
    }

    fn read_system_clock(&self, format: &str) -> Result<String, EvalError> {
        let output = Command::new("date")
            .arg(format!("+{format}"))
            .output()
            .map_err(|err| EvalError::new(format!("failed to run 'date': {err}")))?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(EvalError::new(format!(
                "'date' command failed with status {}: {}",
                output.status,
                stderr.trim()
            )));
        }

        let text = String::from_utf8(output.stdout)
            .map_err(|_| EvalError::new("'date' command returned non-utf8 output"))?;
        Ok(text.trim().to_string())
    }

    fn resolve_runtime_path(&self, path: &str) -> PathBuf {
        let raw = PathBuf::from(path);
        if raw.is_absolute() {
            raw
        } else {
            self.base_dir.join(raw)
        }
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
        }
    }
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}

fn install_builtins(env: &Environment) {
    env.define("input", Value::Builtin(BuiltinFunction::Input));
    // Convenience alias for mirror-brain users.
    env.define("tupni", Value::Builtin(BuiltinFunction::Input));
    env.define("len", Value::Builtin(BuiltinFunction::Len));
    env.define("nel", Value::Builtin(BuiltinFunction::Len));
    env.define("to_number", Value::Builtin(BuiltinFunction::ToNumber));
    env.define("rebmun_ot", Value::Builtin(BuiltinFunction::ToNumber));
    env.define("range", Value::Builtin(BuiltinFunction::Range));
    env.define("egnar", Value::Builtin(BuiltinFunction::Range));
    env.define("type_of", Value::Builtin(BuiltinFunction::TypeOf));
    env.define("fo_epyt", Value::Builtin(BuiltinFunction::TypeOf));
    env.define("to_string", Value::Builtin(BuiltinFunction::ToString));
    env.define("gnirts_ot", Value::Builtin(BuiltinFunction::ToString));
    env.define("__emit_time", Value::Builtin(BuiltinFunction::EmitTime));
    env.define("__lamron_time", Value::Builtin(BuiltinFunction::LamronTime));
    env.define("__etad_today", Value::Builtin(BuiltinFunction::EtadToday));
    env.define("__read_file", Value::Builtin(BuiltinFunction::ReadFile));
    env.define("__write_file", Value::Builtin(BuiltinFunction::WriteFile));
    env.define("__delete_file", Value::Builtin(BuiltinFunction::DeleteFile));
    env.define(
        "__http_request",
        Value::Builtin(BuiltinFunction::HttpRequest),
    );
}

fn iterable_to_values(iterable: Value) -> Result<Vec<Value>, EvalError> {
    match iterable {
        Value::Array(values) => Ok(values),
        Value::String(text) => Ok(text
            .chars()
            .map(|ch| Value::String(ch.to_string()))
            .collect()),
        other => Err(EvalError::new(format!(
            "for loop iterable must be array or string, got '{}'",
            other
        ))),
    }
}

fn eval_builtin_len(args: Vec<Value>) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::new(format!(
            "len expected 1 argument(s), got {}",
            args.len()
        )));
    }

    match &args[0] {
        Value::String(text) => Ok(Value::Number(text.chars().count() as f64)),
        Value::Array(values) => Ok(Value::Number(values.len() as f64)),
        other => Err(EvalError::new(format!(
            "len expects string or array, got '{}'",
            other
        ))),
    }
}

fn eval_builtin_to_number(args: Vec<Value>) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::new(format!(
            "to_number expected 1 argument(s), got {}",
            args.len()
        )));
    }

    match &args[0] {
        Value::Number(number) => Ok(Value::Number(*number)),
        Value::Boolean(value) => Ok(Value::Number(if *value { 1.0 } else { 0.0 })),
        Value::String(text) => {
            let parsed = text.trim().parse::<f64>().map_err(|_| {
                EvalError::new(format!("to_number could not parse '{}' as number", text))
            })?;
            Ok(Value::Number(parsed))
        }
        other => Err(EvalError::new(format!(
            "to_number expects number, boolean, or string, got '{}'",
            other
        ))),
    }
}

fn eval_builtin_range(args: Vec<Value>) -> Result<Value, EvalError> {
    if args.is_empty() || args.len() > 3 {
        return Err(EvalError::new(format!(
            "range expected 1 to 3 argument(s), got {}",
            args.len()
        )));
    }

    let to_i64 = |value: &Value| -> Result<i64, EvalError> {
        let Value::Number(number) = value else {
            return Err(EvalError::new("range arguments must be numbers"));
        };

        if !number.is_finite() || number.fract() != 0.0 {
            return Err(EvalError::new("range arguments must be finite integers"));
        }

        if *number < i64::MIN as f64 || *number > i64::MAX as f64 {
            return Err(EvalError::new("range argument is out of integer bounds"));
        }

        Ok(*number as i64)
    };

    let (start, end, step) = match args.len() {
        1 => (0, to_i64(&args[0])?, 1),
        2 => (to_i64(&args[0])?, to_i64(&args[1])?, 1),
        3 => (to_i64(&args[0])?, to_i64(&args[1])?, to_i64(&args[2])?),
        _ => unreachable!(),
    };

    if step == 0 {
        return Err(EvalError::new("range step cannot be zero"));
    }

    let mut values = Vec::new();
    let mut current = start;
    if step > 0 {
        while current < end {
            values.push(Value::Number(current as f64));
            current = current.saturating_add(step);
        }
    } else {
        while current > end {
            values.push(Value::Number(current as f64));
            current = current.saturating_add(step);
        }
    }

    Ok(Value::Array(values))
}

fn eval_builtin_type_of(args: Vec<Value>) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::new(format!(
            "type_of expected 1 argument(s), got {}",
            args.len()
        )));
    }

    let name = match &args[0] {
        Value::Number(_) => "number",
        Value::Boolean(_) => "boolean",
        Value::String(_) => "string",
        Value::Array(_) => "array",
        Value::Function(_) => "function",
        Value::Builtin(_) => "builtin",
        Value::Null => "null",
    };

    Ok(Value::String(name.to_string()))
}

fn eval_builtin_to_string(args: Vec<Value>) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::new(format!(
            "to_string expected 1 argument(s), got {}",
            args.len()
        )));
    }

    if let Value::String(text) = &args[0] {
        return Ok(Value::String(text.clone()));
    }

    Ok(Value::String(args[0].to_string()))
}

fn normalize_index(index: f64, len: usize, label: &'static str) -> Result<usize, EvalError> {
    if !index.is_finite() || index.fract() != 0.0 {
        return Err(EvalError::new(format!(
            "{label} index must be a finite integer"
        )));
    }

    if len == 0 {
        return Err(EvalError::new(format!("{label} index out of bounds")));
    }

    let len_i64 =
        i64::try_from(len).map_err(|_| EvalError::new(format!("{label} is too large to index")))?;

    if index < i64::MIN as f64 || index > i64::MAX as f64 {
        return Err(EvalError::new(format!(
            "{label} index out of integer bounds"
        )));
    }

    let raw = index as i64;
    let normalized = if raw < 0 { len_i64 + raw } else { raw };

    if normalized < 0 || normalized >= len_i64 {
        return Err(EvalError::new(format!("{label} index out of bounds")));
    }

    usize::try_from(normalized)
        .map_err(|_| EvalError::new(format!("{label} index conversion failed")))
}

fn expect_string_arg(
    value: &Value,
    builtin: &'static str,
    position: usize,
) -> Result<String, EvalError> {
    match value {
        Value::String(text) => Ok(text.clone()),
        other => Err(EvalError::new(format!(
            "{builtin} argument {position} must be a string, got '{}'",
            other
        ))),
    }
}

fn swap_hhmm(input: &str) -> Result<String, EvalError> {
    let (hours, minutes) = input
        .split_once(':')
        .ok_or_else(|| EvalError::new(format!("expected time in HH:MM format, got '{input}'")))?;

    let is_valid = hours.len() == 2
        && minutes.len() == 2
        && hours.chars().all(|ch| ch.is_ascii_digit())
        && minutes.chars().all(|ch| ch.is_ascii_digit());
    if !is_valid {
        return Err(EvalError::new(format!(
            "expected time in HH:MM format, got '{input}'"
        )));
    }

    Ok(format!("{minutes}:{hours}"))
}

#[derive(Debug)]
struct ParsedHttpUrl {
    scheme: String,
    host: String,
    port: u16,
    path: String,
}

#[derive(Debug)]
struct HttpResponse {
    status: u16,
    body: String,
}

fn send_http_request(method: &str, url: &str, body: &str) -> Result<HttpResponse, EvalError> {
    let method = method.trim().to_uppercase();
    if method.is_empty() || !method.chars().all(|ch| ch.is_ascii_uppercase()) {
        return Err(EvalError::new(format!(
            "unsupported HTTP method '{}'",
            method
        )));
    }

    let parsed = parse_http_url(url)?;
    match parsed.scheme.as_str() {
        "http" => send_http_plain(&method, &parsed, body),
        "https" => send_https_via_openssl(&method, &parsed, body),
        _ => Err(EvalError::new(format!(
            "unsupported URL scheme '{}'",
            parsed.scheme
        ))),
    }
}

fn parse_http_url(url: &str) -> Result<ParsedHttpUrl, EvalError> {
    let (scheme, rest) = url
        .split_once("://")
        .ok_or_else(|| EvalError::new(format!("invalid URL '{}': missing scheme", url)))?;
    let scheme = scheme.to_lowercase();
    if scheme != "http" && scheme != "https" {
        return Err(EvalError::new(format!(
            "invalid URL '{}': scheme must be http or https",
            url
        )));
    }

    let (host_port, path) = match rest.split_once('/') {
        Some((host_port, tail)) => (host_port, format!("/{}", tail)),
        None => (rest, "/".to_string()),
    };

    if host_port.is_empty() || host_port.contains('@') {
        return Err(EvalError::new(format!(
            "invalid URL '{}': unsupported authority segment",
            url
        )));
    }

    let (host, port) = match host_port.rsplit_once(':') {
        Some((host, raw_port))
            if !host.contains(']') && raw_port.chars().all(|ch| ch.is_ascii_digit()) =>
        {
            let port = raw_port.parse::<u16>().map_err(|_| {
                EvalError::new(format!("invalid port '{}' in URL '{}'", raw_port, url))
            })?;
            (host.to_string(), port)
        }
        _ => {
            let default_port = if scheme == "https" { 443 } else { 80 };
            (host_port.to_string(), default_port)
        }
    };

    if host.is_empty() {
        return Err(EvalError::new(format!(
            "invalid URL '{}': missing host",
            url
        )));
    }

    Ok(ParsedHttpUrl {
        scheme,
        host,
        port,
        path,
    })
}

fn send_http_plain(
    method: &str,
    parsed: &ParsedHttpUrl,
    body: &str,
) -> Result<HttpResponse, EvalError> {
    let address = format!("{}:{}", parsed.host, parsed.port);
    let socket_addr = address
        .to_socket_addrs()
        .map_err(|err| EvalError::new(format!("failed to resolve '{}': {}", address, err)))?
        .next()
        .ok_or_else(|| EvalError::new(format!("failed to resolve '{}': no addresses", address)))?;

    let mut stream = TcpStream::connect_timeout(&socket_addr, Duration::from_secs(10))
        .map_err(|err| EvalError::new(format!("failed to connect to '{}': {}", address, err)))?;
    stream
        .set_read_timeout(Some(Duration::from_secs(10)))
        .map_err(|err| EvalError::new(format!("failed to set read timeout: {}", err)))?;
    stream
        .set_write_timeout(Some(Duration::from_secs(10)))
        .map_err(|err| EvalError::new(format!("failed to set write timeout: {}", err)))?;

    let request = build_http_request(method, parsed, body);
    stream
        .write_all(request.as_bytes())
        .map_err(|err| EvalError::new(format!("failed to write HTTP request: {}", err)))?;
    stream
        .flush()
        .map_err(|err| EvalError::new(format!("failed to flush HTTP request: {}", err)))?;

    let mut raw = String::new();
    stream
        .read_to_string(&mut raw)
        .map_err(|err| EvalError::new(format!("failed to read HTTP response: {}", err)))?;

    parse_http_response(&raw)
}

fn send_https_via_openssl(
    method: &str,
    parsed: &ParsedHttpUrl,
    body: &str,
) -> Result<HttpResponse, EvalError> {
    let connect_target = format!("{}:{}", parsed.host, parsed.port);
    let request = build_http_request(method, parsed, body);

    let mut child = Command::new("openssl")
        .arg("s_client")
        .arg("-quiet")
        .arg("-connect")
        .arg(&connect_target)
        .arg("-servername")
        .arg(&parsed.host)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|err| {
            EvalError::new(format!(
                "failed to start openssl for HTTPS request: {} (is 'openssl' installed?)",
                err
            ))
        })?;

    {
        let stdin = child
            .stdin
            .as_mut()
            .ok_or_else(|| EvalError::new("failed to open stdin for openssl process"))?;
        stdin
            .write_all(request.as_bytes())
            .map_err(|err| EvalError::new(format!("failed to send HTTPS request: {}", err)))?;
    }

    let output = child
        .wait_with_output()
        .map_err(|err| EvalError::new(format!("failed to read HTTPS response: {}", err)))?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(EvalError::new(format!(
            "openssl HTTPS request failed: {}",
            stderr.trim()
        )));
    }

    let raw = String::from_utf8(output.stdout)
        .map_err(|_| EvalError::new("HTTPS response contained non-utf8 data"))?;
    parse_http_response(&raw)
}

fn build_http_request(method: &str, parsed: &ParsedHttpUrl, body: &str) -> String {
    let host_header = if (parsed.scheme == "http" && parsed.port == 80)
        || (parsed.scheme == "https" && parsed.port == 443)
    {
        parsed.host.clone()
    } else {
        format!("{}:{}", parsed.host, parsed.port)
    };

    let mut request = format!(
        "{method} {} HTTP/1.1\r\nHost: {host_header}\r\nUser-Agent: tfel/0.1\r\nAccept: */*\r\nConnection: close\r\n",
        parsed.path
    );
    if !body.is_empty() {
        request.push_str("Content-Type: text/plain; charset=utf-8\r\n");
        request.push_str(&format!("Content-Length: {}\r\n", body.len()));
    }
    request.push_str("\r\n");
    request.push_str(body);
    request
}

fn parse_http_response(raw: &str) -> Result<HttpResponse, EvalError> {
    let trimmed = if let Some(idx) = raw.find("HTTP/") {
        &raw[idx..]
    } else {
        raw
    };

    let (header_text, body) = if let Some((h, b)) = trimmed.split_once("\r\n\r\n") {
        (h, b.to_string())
    } else if let Some((h, b)) = trimmed.split_once("\n\n") {
        (h, b.to_string())
    } else {
        return Err(EvalError::new(
            "invalid HTTP response: missing header/body separator",
        ));
    };

    let mut lines = header_text.lines();
    let status_line = lines
        .next()
        .ok_or_else(|| EvalError::new("invalid HTTP response: missing status line"))?;
    let status = status_line
        .split_whitespace()
        .nth(1)
        .ok_or_else(|| EvalError::new("invalid HTTP response: malformed status line"))?
        .parse::<u16>()
        .map_err(|_| EvalError::new("invalid HTTP response: status code is not a number"))?;

    Ok(HttpResponse { status, body })
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
        Value::Function(_) => true,
        Value::Builtin(_) => true,
    }
}
