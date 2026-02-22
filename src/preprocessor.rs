#[derive(Debug, Clone)]
pub struct SourceMap {
    output_to_input: Vec<usize>,
    input_len: usize,
}

impl SourceMap {
    pub fn map_offset(&self, output_offset: usize) -> usize {
        if self.output_to_input.is_empty() {
            return 0;
        }

        if output_offset >= self.output_to_input.len() {
            return self.input_len;
        }

        self.output_to_input[output_offset]
    }

    pub fn map_span(&self, start: usize, end: usize) -> (usize, usize) {
        (self.map_offset(start), self.map_offset(end))
    }
}

#[derive(Debug, Clone)]
pub struct PreprocessOutput {
    pub source: String,
    pub source_map: SourceMap,
}

pub fn preprocess_source_with_map(input: &str) -> PreprocessOutput {
    #[derive(Clone)]
    struct DecodedLine {
        text: String,
        map: Vec<usize>,
        start: usize,
        newline_byte: Option<usize>,
    }

    let mut decoded_lines = Vec::<DecodedLine>::new();
    let mut line_start = 0usize;

    for (idx, ch) in input.char_indices() {
        if ch == '\n' {
            let line = &input[line_start..idx];
            let (decoded, map) = reverse_line_preserving_string_literals_with_map(line, line_start);
            decoded_lines.push(DecodedLine {
                text: decoded,
                map,
                start: line_start,
                newline_byte: Some(idx),
            });
            line_start = idx + 1;
        }
    }

    if line_start < input.len() {
        let line = &input[line_start..];
        let (decoded, map) = reverse_line_preserving_string_literals_with_map(line, line_start);
        decoded_lines.push(DecodedLine {
            text: decoded,
            map,
            start: line_start,
            newline_byte: None,
        });
    }

    decoded_lines.reverse();

    let mut output = String::new();
    let mut output_to_input = Vec::<usize>::new();

    for (idx, line) in decoded_lines.iter().enumerate() {
        output.push_str(&line.text);
        output_to_input.extend_from_slice(&line.map);

        if idx + 1 < decoded_lines.len() {
            output.push('\n');
            output_to_input.push(line.newline_byte.unwrap_or(line.start));
        }
    }

    if input.ends_with('\n') {
        output.push('\n');
        let fallback = input.len().saturating_sub(1);
        let mapped = decoded_lines
            .last()
            .and_then(|line| line.newline_byte)
            .unwrap_or(fallback);
        output_to_input.push(mapped);
    }

    PreprocessOutput {
        source: output,
        source_map: SourceMap {
            output_to_input,
            input_len: input.len(),
        },
    }
}

pub fn preprocess_source(input: &str) -> String {
    preprocess_source_with_map(input).source
}

fn reverse_line_preserving_string_literals_with_map(
    line: &str,
    line_start: usize,
) -> (String, Vec<usize>) {
    #[derive(Clone)]
    struct Segment {
        chars: Vec<(char, usize)>,
        is_string: bool,
    }

    let mut segments = Vec::<Segment>::new();
    let mut current = Vec::<(char, usize)>::new();
    let mut in_string = false;
    let mut escaped = false;

    for (idx, ch) in line.char_indices() {
        let source_byte = line_start + idx;

        if in_string {
            current.push((ch, source_byte));
            if escaped {
                escaped = false;
                continue;
            }
            if ch == '\\' {
                escaped = true;
                continue;
            }
            if ch == '"' {
                segments.push(Segment {
                    chars: current,
                    is_string: true,
                });
                current = Vec::new();
                in_string = false;
            }
            continue;
        }

        if ch == '"' {
            if !current.is_empty() {
                segments.push(Segment {
                    chars: current,
                    is_string: false,
                });
                current = Vec::new();
            }
            current.push((ch, source_byte));
            in_string = true;
            continue;
        }

        current.push((ch, source_byte));
    }

    if !current.is_empty() {
        segments.push(Segment {
            chars: current,
            is_string: in_string,
        });
    }

    let mut out = String::new();
    let mut map = Vec::<usize>::new();

    for segment in segments.into_iter().rev() {
        let mut chars = segment.chars;
        if !segment.is_string {
            chars.reverse();
        }

        for (ch, source_byte) in chars {
            out.push(ch);
            map.extend(std::iter::repeat(source_byte).take(ch.len_utf8()));
        }
    }

    (out, map)
}
