// SPDX-License-Identifier: GPL-3.0-or-later OR AGPL-3.0-or-later
// Copyright (C) 2025  Red Hat, Inc.

use either::Either;
use std::io::Write;
use textdistance::Algorithm;

use crate::Args;
use crate::FileRanges;

fn quote_char(c: char, in_range: bool) -> String {
    match c {
        '\t' => "\\t".to_string(),
        '\r' => "\\r".to_string(),
        '\n' => "\\n".to_string(),
        '"' | '\\' => format!("\\{}", c),
        '[' | ']' if in_range => format!("\\{}", c),
        _ => c.to_string(),
    }
}

fn negative(control_line: Option<&String>, separator: &str) -> String {
    let mut rule = String::new();

    let mut chars_sep = separator.chars().collect::<Vec<char>>();
    chars_sep.push('\n');
    let chars = match control_line {
        Some(s) => {
            let mut chars = s.chars().collect::<Vec<char>>();
            chars.push('\n');
            chars
        }
        None => Vec::new(),
    };

    let nr = chars.len().max(chars_sep.len());
    rule.push_str("( ( (\n ");
    for i in 0..nr {
        let mut positive = String::new();
        let mut positive_sep = String::new();
        for (j, (c, s)) in chars.iter().zip(chars_sep.iter()).enumerate().take(i + 1) {
            if i == j {
                let c = if *c != '\n' {
                    &quote_char(*c, true)
                } else {
                    ""
                };
                let s = if *s != '\n' {
                    &quote_char(*s, true)
                } else {
                    ""
                };
                let positive = if positive.is_empty() {
                    ""
                } else {
                    &format!("\"{}\" ", positive)
                };
                let positive_sep = if positive_sep.is_empty() {
                    ""
                } else {
                    &format!("\"{}\" ", positive_sep)
                };
                if i > 0 {
                    rule.push_str(&format!(
                        "{}[^\\n{}] |\n {}[^\\n{}]",
                        positive, c, positive_sep, s
                    ));
                } else {
                    rule.push_str(&format!("[^\\n{}{}]", c, s));
                }
            } else {
                positive.push_str(&quote_char(*c, false));
                positive_sep.push_str(&quote_char(*s, false));
            }
        }
        if chars.len() == chars_sep.len() {
            continue;
        }
        let (chars, mut positive, short_len) = if chars.len() > chars_sep.len() {
            (&chars, positive, chars_sep.len())
        } else {
            (&chars_sep, positive_sep, chars.len())
        };

        for (j, c) in chars.iter().enumerate().take(i + 1) {
            if j < short_len {
                continue;
            }
            if i == j {
                let c = if *c != '\n' {
                    &quote_char(*c, true)
                } else {
                    ""
                };
                let positive = if positive.is_empty() {
                    ""
                } else {
                    &format!("\"{}\" ", positive)
                };
                rule.push_str(&format!("{}[^\\n{}]", positive, c));
            } else {
                positive.push_str(&quote_char(*c, false));
            }
        }
        if i < nr - 1 {
            rule.push_str(" |\n ");
        }
    }
    rule.push_str(") [^\\n]* )? [\\n] )*\n");
    rule
}

macro_rules! filter_unique {
    ($iter:expr, $lines:expr) => {
        $iter.filter(|line| $lines.iter().filter(|x| x == line).count() == 1)
    };
}

pub fn find_distant_control_line<'a>(
    lines: &'a [String],
    control_lines: &'a [String],
    after: bool,
) -> Option<&'a String> {
    let unique_control_lines: Vec<_> = filter_unique!(control_lines.iter(), lines).collect();
    if unique_control_lines.len() == 1 {
        return Some(unique_control_lines[0]);
    }
    let different_control_lines: Vec<String> = {
        let mut seen_lines = std::collections::HashSet::new();
        control_lines
            .iter()
            .filter(|line| seen_lines.insert(line.to_string()))
            .map(|s| s.to_string())
            .collect()
    };
    let damerau_levenshtein = textdistance::DamerauLevenshtein::default();
    let mut max_dist = 0.0;
    let mut max_line: Option<&String> = None;
    let mut seen = std::collections::HashSet::new();
    let unique_control_lines_iter = if !after {
        Either::Left(unique_control_lines.iter().rev())
    } else {
        Either::Right(unique_control_lines.iter())
    };
    for control_line in unique_control_lines_iter {
        let mut control_max_dist = 0.0;
        for line in &different_control_lines {
            if **control_line == *line {
                continue;
            }
            let key = if control_line < &line {
                (*control_line, line)
            } else {
                (line, *control_line)
            };
            if !seen.insert(key) {
                continue;
            }
            let dist = damerau_levenshtein.for_str(control_line, line).nval();
            if dist > control_max_dist {
                control_max_dist = dist;
            }
        }
        if control_max_dist > max_dist || max_line.is_none() {
            max_dist = control_max_dist;
            max_line = Some(control_line);
        }
    }
    if max_line.is_none() {
        assert!(unique_control_lines.is_empty());
    }
    max_line
}

pub fn find_long_control_line<'a>(
    lines: &'a [String],
    control_lines: &'a [String],
    after: bool,
) -> Option<&'a String> {
    let mut max_len = 0;
    let mut max_line: Option<&String> = None;
    let unique_control_lines_iter = if !after {
        Either::Left(filter_unique!(control_lines.iter(), lines).rev())
    } else {
        Either::Right(filter_unique!(control_lines.iter(), lines))
    };
    for line in unique_control_lines_iter {
        if line.len() > max_len || max_line.is_none() {
            max_len = line.len();
            max_line = Some(line);
        }
    }
    max_line
}

pub fn find_control_line<'a>(
    lines: &'a [String],
    control_lines: &'a [String],
    after: bool,
) -> Option<&'a String> {
    let control_line = if false {
        find_long_control_line(lines, control_lines, after)
    } else {
        find_distant_control_line(lines, control_lines, after)
    };
    if let Some(found) = control_line {
        assert!(control_lines.contains(found));
    } else if !after {
        // use the last dup as "start" control line
        return control_lines.last();
    }
    control_line
}

fn push_control_line(file_rule: &mut String, control_line: &str) {
    file_rule.push_str(" \"");
    file_rule.push_str(
        &control_line
            .chars()
            .map(|c| quote_char(c, false))
            .collect::<String>(),
    );
    file_rule.push_str("\" [\\n] \n");
}

pub fn generate_gbnf_file(
    file_ranges: &mut FileRanges,
    output_path: &std::path::Path,
    args: &Args,
) -> Result<Option<tempfile::NamedTempFile>, anyhow::Error> {
    let mut file_rules = Vec::new();
    let before_context = args.context.max(args.before_context) as usize;
    let after_context = args.context.max(args.after_context) as usize;

    for (filename, ranges) in &mut file_ranges.hash {
        assert!(!ranges.is_empty());
        let mut file_rule = String::new();

        file_rule.push_str("prefix \"");
        file_rule.push_str(filename);
        file_rule.push_str("\" \"\\n\"\n");

        for range in ranges {
            // Generate rules for each snippet
            let lines = &range.lines.clone();
            let before_context = range.before_context.saturating_sub(before_context);
            let after_context = range.after_context.saturating_sub(after_context);

            let control_lines_before = &lines[..before_context];
            let control_lines_after = &lines[lines.len() - after_context..];
            let control_line_before = find_control_line(lines, control_lines_before, false);
            let control_line_after = find_control_line(lines, control_lines_after, true);

            let index = if let Some(control_line) = control_line_before {
                push_control_line(&mut file_rule, control_line);

                control_lines_before
                    .iter()
                    .rposition(|line| line == control_line)
                    .unwrap()
            } else {
                control_lines_before.len()
            };
            range.start += index;
            range.lines.drain(..index);

            let index = if let Some(control_line) = control_line_after {
                control_lines_after.len()
                    - control_lines_after
                        .iter()
                        .position(|line| line == &control_line.to_string())
                        .unwrap()
                    - 1
            } else {
                control_lines_after.len()
            };
            range.end -= index;
            range.lines.drain(range.lines.len() - index..);

            assert!(range.end - range.start == range.lines.len() - 1);

            file_rule.push_str(&negative(control_line_after, &args.context_separator));
            if let Some(control_line) = control_line_after {
                file_rule.push_str(" ( ");
                push_control_line(&mut file_rule, control_line);
                file_rule.push_str(" )?\n");
            }
            file_rule.push_str(" separator\n");
        }
        file_rule.push_str(" [\\n]?");

        file_rules.push(file_rule);
    }

    let mut gbnf_content = String::new();
    gbnf_content.push_str("root ::= \"```\\n\" ( ");
    gbnf_content.push_str(&file_rules.join(" |\n"));
    gbnf_content.push_str(&format!(
        " ){{1,{}}} \"```\"\n",
        file_ranges.filenames.len()
    ));

    gbnf_content.push_str("prefix ::= \"");
    gbnf_content.push_str(
        &args
            .filename_prefix
            .chars()
            .map(|c| quote_char(c, false))
            .collect::<String>(),
    );
    gbnf_content.push_str("\"\n");

    gbnf_content.push_str("separator ::= \"");
    gbnf_content.push_str(
        &args
            .context_separator
            .chars()
            .map(|c| quote_char(c, false))
            .collect::<String>(),
    );
    gbnf_content.push_str("\\n\"");

    let mut file = tempfile::Builder::new()
        .prefix(&format!("{}-", output_path.to_str().unwrap()))
        .suffix(".gbnf")
        .tempfile()?;
    writeln!(file, "{}", gbnf_content)?;
    file.flush()?;

    Ok(Some(file))
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::NamedTempFile;

    #[test]
    fn test_gbnf_generation() {
        // This is a simplified test - in practice, you'd need to mock FileRanges
        let temp_file = NamedTempFile::new().unwrap();
        let temp_path = temp_file.path();

        // Create a mock Args
        let args = Args {
            context_separator: "-- DO NOT DELETE THIS SEPARATOR".to_string(),
            filename_prefix: "-- ".to_string(),
            ..Default::default()
        };

        // This test would require more complex setup to test actual FileRanges
        // For now, we just ensure the function compiles and doesn't panic
        let mut file_ranges = FileRanges::new("", "", "").unwrap();
        assert!(generate_gbnf_file(&mut file_ranges, temp_path, &args).is_ok());
    }
}

// Local Variables:
// rust-format-on-save: t
// End:
