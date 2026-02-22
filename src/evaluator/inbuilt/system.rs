use super::*;
use std::fs;
use std::io::{self, Write};
use std::path::PathBuf;
use std::process::Command;

impl Evaluator {
    pub(super) fn eval_builtin_input(&mut self, args: Vec<Value>) -> Result<Value, EvalError> {
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

    pub(super) fn eval_builtin_emit_time(&mut self, args: Vec<Value>) -> Result<Value, EvalError> {
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

    pub(super) fn eval_builtin_lamron_time(
        &mut self,
        args: Vec<Value>,
    ) -> Result<Value, EvalError> {
        if !args.is_empty() {
            return Err(EvalError::new(format!(
                "__lamron_time expected 0 argument(s), got {}",
                args.len()
            )));
        }
        Ok(Value::String(self.read_system_clock("%H:%M")?))
    }

    pub(super) fn eval_builtin_etad_today(&mut self, args: Vec<Value>) -> Result<Value, EvalError> {
        if !args.is_empty() {
            return Err(EvalError::new(format!(
                "__etad_today expected 0 argument(s), got {}",
                args.len()
            )));
        }
        Ok(Value::String(self.read_system_clock("%Y-%m-%d")?))
    }

    pub(super) fn eval_builtin_read_file(&mut self, args: Vec<Value>) -> Result<Value, EvalError> {
        if !self.options.permissions.allow_fs {
            return Err(EvalError::new(
                "filesystem access is disabled (pass --allow-fs to enable)",
            ));
        }

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

    pub(super) fn eval_builtin_write_file(&mut self, args: Vec<Value>) -> Result<Value, EvalError> {
        if !self.options.permissions.allow_fs {
            return Err(EvalError::new(
                "filesystem access is disabled (pass --allow-fs to enable)",
            ));
        }

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

    pub(super) fn eval_builtin_delete_file(
        &mut self,
        args: Vec<Value>,
    ) -> Result<Value, EvalError> {
        if !self.options.permissions.allow_fs {
            return Err(EvalError::new(
                "filesystem access is disabled (pass --allow-fs to enable)",
            ));
        }

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
}

pub(super) fn expect_string_arg(
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
