use tfel::preprocessor::{preprocess_source, preprocess_source_with_map};

#[test]
fn reverses_each_line_for_single_line_input() {
    let src = "cba";
    let out = preprocess_source(src);
    assert_eq!(out, "abc");
}

#[test]
fn decodes_lines_and_reverses_execution_order() {
    let src = "eno\nowt\n";
    let out = preprocess_source(src);
    assert_eq!(out, "two\none\n");
}

#[test]
fn double_preprocess_roundtrips_source() {
    let src = "alpha\nbeta\n\ngamma\n";
    let roundtrip = preprocess_source(&preprocess_source(src));
    assert_eq!(roundtrip, src);
}

#[test]
fn preserves_string_literal_content_when_mirroring() {
    let mirrored = ";lru = \"https://api.github.com/zen\"\n";
    let logical = preprocess_source(mirrored);
    assert_eq!(logical, "\"https://api.github.com/zen\" = url;\n");
}

#[test]
fn preserves_escaped_quotes_inside_strings() {
    let logical = "print)\"a \\\"quoted\\\" value\"(;";
    let roundtrip = preprocess_source(&preprocess_source(logical));
    assert_eq!(roundtrip, logical);
}

#[test]
fn source_map_points_back_to_original_bytes() {
    let mirrored = ";1 = x\n;2 = y\n";
    let output = preprocess_source_with_map(mirrored);
    assert_eq!(output.source, "y = 2;\nx = 1;\n");

    let y_offset = output.source.find('y').expect("output should contain y");
    let mapped = output.source_map.map_offset(y_offset);
    assert_eq!(&mirrored[mapped..mapped + 1], "y");
}

#[test]
fn preprocess_roundtrip_holds_for_many_generated_inputs() {
    let mut seed = 0x51F15EED_u64;

    for _ in 0..1_000 {
        let source = pseudo_random_preprocess_input(&mut seed);
        let roundtrip = preprocess_source(&preprocess_source(&source));
        assert_eq!(roundtrip, source, "roundtrip mismatch for generated source");
    }
}

#[test]
fn source_map_preserves_byte_identity_for_generated_inputs() {
    let mut seed = 0xDEC0DE_u64;

    for _ in 0..500 {
        let source = pseudo_random_preprocess_input(&mut seed);
        let output = preprocess_source_with_map(&source);

        assert_eq!(output.source.len(), source.len());
        assert_eq!(
            output.source_map.map_offset(output.source.len()),
            source.len()
        );

        for idx in 0..output.source.len() {
            let mapped = output.source_map.map_offset(idx);
            assert!(mapped < source.len(), "mapped byte out of range at {idx}");
            assert_eq!(
                output.source.as_bytes()[idx],
                source.as_bytes()[mapped],
                "mapped byte mismatch at output byte {idx}"
            );
        }
    }
}

fn pseudo_random_preprocess_input(seed: &mut u64) -> String {
    let line_count = (next_u64(seed) % 7 + 1) as usize;
    let mut out = String::new();

    for _ in 0..line_count {
        let line_start = out.len();
        let segment_count = (next_u64(seed) % 6 + 1) as usize;

        for _ in 0..segment_count {
            if next_u64(seed).is_multiple_of(3) {
                out.push('"');
                let literal_len = (next_u64(seed) % 12) as usize;
                for _ in 0..literal_len {
                    match next_u64(seed) % 7 {
                        0 => out.push_str("\\\\"),
                        1 => out.push_str("\\\""),
                        2 => out.push(' '),
                        3 => out.push('/'),
                        _ => out.push((b'a' + (next_u64(seed) % 26) as u8) as char),
                    }
                }
                out.push('"');
            } else {
                let plain_len = (next_u64(seed) % 10) as usize;
                for _ in 0..plain_len {
                    const CHARSET: &[u8] =
                        b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_ ;,+-*/%!<>=(){}[]";
                    let idx = (next_u64(seed) as usize) % CHARSET.len();
                    out.push(CHARSET[idx] as char);
                }
            }
        }

        if out.len() == line_start {
            out.push('x');
        }

        out.push('\n');
    }

    out
}

fn next_u64(seed: &mut u64) -> u64 {
    *seed = seed.wrapping_mul(6364136223846793005).wrapping_add(1);
    *seed
}
