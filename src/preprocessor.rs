pub fn preprocess_source(input: &str) -> String {
    let mut lines = input
        .lines()
        .map(reverse_line_preserving_string_literals)
        .collect::<Vec<_>>();

    lines.reverse();

    let mut output = lines.join("\n");
    if input.ends_with('\n') {
        output.push('\n');
    }

    output
}

fn reverse_line_preserving_string_literals(line: &str) -> String {
    #[derive(Clone)]
    struct Segment {
        text: String,
        is_string: bool,
    }

    let mut segments = Vec::<Segment>::new();
    let mut current = String::new();
    let mut in_string = false;
    let mut escaped = false;

    for ch in line.chars() {
        if in_string {
            current.push(ch);
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
                    text: current,
                    is_string: true,
                });
                current = String::new();
                in_string = false;
            }
            continue;
        }

        if ch == '"' {
            if !current.is_empty() {
                segments.push(Segment {
                    text: current,
                    is_string: false,
                });
                current = String::new();
            }
            current.push(ch);
            in_string = true;
            continue;
        }

        current.push(ch);
    }

    if !current.is_empty() {
        segments.push(Segment {
            text: current,
            is_string: in_string,
        });
    }

    segments
        .into_iter()
        .rev()
        .map(|segment| {
            if segment.is_string {
                segment.text
            } else {
                segment.text.chars().rev().collect()
            }
        })
        .collect()
}
