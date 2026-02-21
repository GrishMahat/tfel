use tfel::evaluator::Evaluator;
use tfel::lexer::tokenize_with_report;
use tfel::parser::Parser;
use tfel::preprocessor::preprocess_source;

#[test]
fn pipeline_survives_random_garbage_inputs() {
    let mut seed = 0xC0FFEE1234_u64;

    for _ in 0..1_000 {
        let src = pseudo_random_source(&mut seed, 180);
        let preprocessed = preprocess_source(&src);
        let report = tokenize_with_report(&preprocessed);

        let parsed = Parser::new(report.tokens).parse_program();
        if let Ok(program) = parsed {
            let mut evaluator = Evaluator::new();
            let _ = evaluator.eval_program(&program);
        }
    }
}

fn pseudo_random_source(seed: &mut u64, max_len: usize) -> String {
    const CHARSET: &[u8] =
        b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_ \n\t;,+-*/%!<>=(){}[]\"'";

    let len = (next_u64(seed) as usize) % max_len;
    let mut out = String::with_capacity(len);
    for _ in 0..len {
        let idx = (next_u64(seed) as usize) % CHARSET.len();
        out.push(CHARSET[idx] as char);
    }
    out
}

fn next_u64(seed: &mut u64) -> u64 {
    *seed = seed.wrapping_mul(6364136223846793005).wrapping_add(1);
    *seed
}
