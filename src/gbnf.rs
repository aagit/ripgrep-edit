// SPDX-License-Identifier: GPL-3.0-or-later OR AGPL-3.0-or-later
// Copyright (C) 2025  Red Hat, Inc.

use std::io::Write;

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

fn negative(string: &[String], separator: &str) -> String {
    let mut rule = String::new();
    rule.push_str("( ");
    let chars = string.join("\n").chars().collect::<Vec<char>>();
    let chars_sep = separator.chars().collect::<Vec<char>>();
    let nr = chars.len().max(chars_sep.len());
    for i in 1..=nr {
        let mut positive = String::new();
        let mut positive_sep = String::new();
        for (j, (c, s)) in chars.iter().zip(chars_sep.iter()).enumerate().take(i) {
            if i == j + 1 {
                let c = quote_char(*c, true);
                let s = quote_char(*s, true);
                if j > 0 {
                    rule.push_str(&format!(
                        "\"{}\" [^{}] | \"{}\" [^{}]",
                        positive, c, positive_sep, s
                    ));
                } else {
                    rule.push_str(&format!("[^{}{}]", c, s));
                }
            } else {
                positive.push_str(&quote_char(*c, false));
                positive_sep.push_str(&quote_char(*s, false));
            }
        }
        if chars.len() > chars_sep.len() {
            for (j, c) in chars.iter().enumerate().take(i) {
                if j < chars_sep.len() {
                    continue;
                }
                if i == j + 1 {
                    let c = quote_char(*c, true);
                    if j > 0 {
                        rule.push_str(&format!("\"{}\" ", positive));
                    }
                    rule.push_str(&format!("[^{}]", c));
                } else {
                    positive.push_str(&quote_char(*c, false));
                }
            }
        } else if chars.len() < chars_sep.len() {
            for (j, s) in chars_sep.iter().enumerate().take(i) {
                if j < chars.len() {
                    continue;
                }
                if i == j + 1 {
                    let s = quote_char(*s, true);
                    if j > 0 {
                        rule.push_str(&format!("\"{}\" ", positive_sep));
                    }
                    rule.push_str(&format!("[^{}]", s));
                } else {
                    positive_sep.push_str(&quote_char(*s, false));
                }
            }
        }
        if i < nr {
            rule.push_str(" | ");
        }
    }
    rule.push_str(" )*");
    rule
}

pub fn generate_gbnf_file(
    file_ranges: &FileRanges,
    args: &Args,
    output_path: &std::ffi::OsStr,
) -> Result<tempfile::NamedTempFile, anyhow::Error> {
    let mut file = tempfile::Builder::new()
        .prefix(&format!("{}-", output_path.to_str().unwrap()))
        .suffix(".gbnf")
        .tempfile()?;

    let mut file_rules = Vec::new();
    let max_window = args.context.max(args.after_context) as usize;

    for (filename, ranges) in &file_ranges.hash {
        let mut file_rule = String::new();

        let mut window = 1;
        let mut has_dups;

        loop {
            has_dups = false;
            for range in ranges {
                let lines = &range.lines;

                if lines.len() <= window || window > max_window {
                    has_dups = true;
                    window = 0;
                    break;
                }

                let start_idx = lines.len() - window;
                let context = &lines[start_idx..];

                // Scan all windows ending before the final context window starts
                for w_lines in lines[..lines.len() - 1].windows(window) {
                    if w_lines == context {
                        has_dups = true;
                        break;
                    }
                }

                if has_dups {
                    break;
                }
            }
            if window == 0 || !has_dups {
                break;
            }
            window += 1;
        }
        assert!((window == 0) == has_dups);

        if args.dump_on_error {
            match window {
                0 => eprintln!(
                    "Filename {} has grammar disabled due to duplicate snippets",
                    filename
                ),
                1 => {}
                _ => eprintln!(
                    "Filename {} has grammar window {} and context {}, consider growing -C/-A",
                    filename, window, max_window
                ),
            }
        }

        file_rule.push_str("prefix \"");
        file_rule.push_str(filename);
        file_rule.push_str("\" \"\\n\" ");

        for range in ranges {
            // Generate rules for each snippet
            let lines = &range.lines;
            let start_idx = lines.len() - window;
            assert!(start_idx > 0);
            let control_lines = &lines[start_idx..];

            file_rule.push_str(" \"");
            for c in lines[0].chars() {
                file_rule.push_str(&quote_char(c, false));
            }
            file_rule.push_str("\" ");

            file_rule.push_str(&negative(
                &if !has_dups {
                    control_lines
                        .iter()
                        .cloned()
                        .chain(std::iter::once(String::new()))
                        .collect::<Vec<_>>()
                } else {
                    Vec::new()
                },
                &format!("{}\n", &args.context_separator),
            ));

            if !has_dups {
                file_rule.push_str(" (");
                for line in control_lines {
                    if !line.is_empty() {
                        file_rule.push_str(" \"");
                        for c in line.chars() {
                            file_rule.push_str(&quote_char(c, false));
                        }
                        file_rule.push('"');
                    }
                    file_rule.push_str(" \"\\n\"");
                }
                file_rule.push_str(" )?");
            }
            file_rule.push_str(" separator ");
        }
        file_rule.push_str(" [\\n]?");

        file_rules.push(file_rule);
    }

    let mut gbnf_content = String::new();
    gbnf_content.push_str("root ::= \"```\\n\" ( ");
    gbnf_content.push_str(&file_rules.join(" | "));
    gbnf_content.push_str(&format!(
        " ){{1,{}}} \"```\"\n",
        file_ranges.filenames.len()
    ));

    gbnf_content.push_str("prefix ::= \"");
    gbnf_content.push_str(&args.filename_prefix);
    gbnf_content.push_str("\"\n");

    gbnf_content.push_str("separator ::= \"");
    gbnf_content.push_str(&args.context_separator);
    gbnf_content.push_str("\" \"\\n\"");

    // Write to file
    writeln!(file, "{}", gbnf_content)?;
    file.flush()?;

    Ok(file)
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
        let file_ranges = FileRanges::new("", "", "").unwrap();
        assert!(generate_gbnf_file(&file_ranges, &args, temp_path.as_os_str()).is_ok());
    }
}

// Local Variables:
// rust-format-on-save: t
// End:
