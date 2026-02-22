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
