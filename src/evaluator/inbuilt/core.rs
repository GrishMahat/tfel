use super::*;

pub(super) fn iterable_to_values(iterable: Value) -> Result<Vec<Value>, EvalError> {
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

pub(super) fn eval_builtin_len(args: Vec<Value>) -> Result<Value, EvalError> {
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

pub(super) fn eval_builtin_to_number(args: Vec<Value>) -> Result<Value, EvalError> {
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

pub(super) fn eval_builtin_range(args: Vec<Value>) -> Result<Value, EvalError> {
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

pub(super) fn eval_builtin_type_of(args: Vec<Value>) -> Result<Value, EvalError> {
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
        Value::Object(_) => "object",
        Value::Function(_) => "function",
        Value::Builtin(_) => "builtin",
        Value::Null => "null",
    };

    Ok(Value::String(name.to_string()))
}

pub(super) fn eval_builtin_to_string(args: Vec<Value>) -> Result<Value, EvalError> {
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

pub(super) fn normalize_index(
    index: f64,
    len: usize,
    label: &'static str,
) -> Result<usize, EvalError> {
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
