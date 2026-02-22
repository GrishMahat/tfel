use super::*;

mod core;
mod http;
mod system;

pub(super) fn iterable_to_values(iterable: Value) -> Result<Vec<Value>, EvalError> {
    core::iterable_to_values(iterable)
}

pub(super) fn normalize_index(
    index: f64,
    len: usize,
    label: &'static str,
) -> Result<usize, EvalError> {
    core::normalize_index(index, len, label)
}

pub(super) fn install_builtins(env: &Environment) {
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

impl Evaluator {
    pub(super) fn eval_builtin_call(
        &mut self,
        builtin: BuiltinFunction,
        args: Vec<Value>,
    ) -> Result<Value, EvalError> {
        match builtin {
            BuiltinFunction::Input => self.eval_builtin_input(args),
            BuiltinFunction::Len => core::eval_builtin_len(args),
            BuiltinFunction::ToNumber => core::eval_builtin_to_number(args),
            BuiltinFunction::Range => core::eval_builtin_range(args),
            BuiltinFunction::TypeOf => core::eval_builtin_type_of(args),
            BuiltinFunction::ToString => core::eval_builtin_to_string(args),
            BuiltinFunction::EmitTime => self.eval_builtin_emit_time(args),
            BuiltinFunction::LamronTime => self.eval_builtin_lamron_time(args),
            BuiltinFunction::EtadToday => self.eval_builtin_etad_today(args),
            BuiltinFunction::ReadFile => self.eval_builtin_read_file(args),
            BuiltinFunction::WriteFile => self.eval_builtin_write_file(args),
            BuiltinFunction::DeleteFile => self.eval_builtin_delete_file(args),
            BuiltinFunction::HttpRequest => self.eval_builtin_http_request(args),
        }
    }
}
