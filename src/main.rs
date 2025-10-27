// SPDX-License-Identifier: GPL-3.0-or-later OR AGPL-3.0-or-later
// Copyright (C) 2025  Red Hat, Inc.

use clap::Parser;
use std::collections::HashMap;
use std::fs;
use std::fs::File;
use std::io::{BufRead, BufReader, Write};
use std::path::Path;
use std::process::Command;

#[derive(Parser, Debug, Default)]
#[clap(version)]
struct Args {
    /// Regex pattern to search for
    #[clap(short = 'e', long, allow_hyphen_values = true)]
    regexp: String,

    /// Directory or file to search in
    #[clap(short, long)]
    path: String,

    /// Editor command
    #[clap(short = 'E', long)]
    editor: String,

    /// Number of context lines
    #[clap(short = 'C', long, default_value_t = 5, value_parser = clap::value_parser!(u32).range(1..),)]
    context: u32,

    /// Number of lines of context to show after each match
    #[clap(short = 'A', long, default_value_t = 0)]
    after_context: u32,

    /// Number of lines of context to show before each match
    #[clap(short = 'B', long, default_value_t = 0)]
    before_context: u32,

    /// Require all files in the ripgrep output to be present in the modified file
    #[clap(long, default_value_t = false)]
    require_all_files: bool,

    /// context_separator character
    #[clap(
        long,
        default_value = "-- DO NOT DELETE THIS SEPARATOR --",
        allow_hyphen_values = true
    )]
    context_separator: String,
}

fn dedup_slashes(line: &str) -> String {
    let mut chars = line.chars().collect::<Vec<char>>();
    chars.dedup_by(|a, b| *a == '/' && *a == *b);
    chars.iter().collect::<String>()
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    // Run ripgrep with context lines
    let context_separator = &args.context_separator;

    let mut rg_cmd = Command::new("rg");
    rg_cmd
        .arg("-n")
        .arg(format!("-C{}", args.context))
        .arg("--heading")
        .arg("--color=never")
        .arg("--field-context-separator=:")
        .arg(format!("--context-separator={context_separator}"))
        .arg("-e")
        .arg(&args.regexp)
        .arg(&args.path);
    if args.after_context > 0 {
        rg_cmd.arg(format!("-A{}", args.after_context));
    }
    if args.before_context > 0 {
        rg_cmd.arg(format!("-B{}", args.before_context));
    }

    let output = rg_cmd.output();
    match output {
        Ok(_) => {}
        Err(e) => {
            if e.kind() == std::io::ErrorKind::NotFound {
                eprintln!("Error: ripgrep is not installed.");
            } else {
                eprintln!("Failed to execute ripgrep: {e}");
            }
            std::process::exit(1);
        }
    }
    let output_str = String::from_utf8_lossy(&output.unwrap().stdout).into_owned();
    if output_str.trim().is_empty() {
        eprintln!("No results found.");
        std::process::exit(0);
    }
    let temp_file = tempfile::Builder::new()
        .prefix("ripgrep-edit-")
        .suffix(".ripgrep-edit")
        .tempfile()?;
    let temp_path = temp_file.path().to_str().unwrap();
    let mut file = temp_file.as_file();

    // Parse the output to extract file paths before writing to temp file
    let lines = output_str.lines().peekable();

    let mut file_ranges: HashMap<String, Vec<(usize, usize)>> = HashMap::new();
    file_ranges.extend(parse_file_ranges(lines, context_separator));

    let mut first_file_written = false;

    for line in output_str.lines() {
        if line.starts_with(|c: char| c.is_ascii_digit()) {
            let colon_pos = line.find(':').unwrap();
            // Remove the "number:" prefix from lines starting with digits
            let after_colon = &line[colon_pos + 1..];
            if after_colon == context_separator {
                panic!("context_separator found after colon in line: {line}");
            }
            writeln!(file, "{after_colon}")?;
        } else {
            let line = dedup_slashes(line);
            // Check if this line matches a file in file_ranges and is not the first file
            let is_file_match = file_ranges.contains_key(&line);
            if is_file_match {
                if first_file_written {
                    writeln!(file, "{context_separator}\n")?;
                } else {
                    first_file_written = true;
                }
            }
            if is_file_match || &line == context_separator {
                writeln!(file, "{line}")?;
            }
        }
    }

    // Write context separator after the last line of the last snippnet
    if first_file_written {
        writeln!(file, "{context_separator}")?;
    }
    file.flush()?;

    // Get modification time before opening editor
    let before_edit = fs::metadata(temp_path)?.modified()?;

    // Open with editor
    let mut editor_cmd = Command::new("sh");
    editor_cmd
        .arg("-c")
        .arg(format!("{} {}", args.editor, temp_path));

    let editor_status = editor_cmd.status()?;

    if !editor_status.success() {
        eprintln!("Editor failed to run");
        return Ok(());
    }

    // Get modification time after editor
    let after_edit = fs::metadata(temp_path)?.modified()?;

    // Check if the file was modified
    if after_edit <= before_edit {
        eprintln!("File was not modified by the editor. Exiting without changes.");
        return Ok(());
    }

    // Read modified file
    let file = File::open(temp_path)?;
    let reader = BufReader::new(file);

    let changes = parse_modified_file(reader, &file_ranges, context_separator)?;
    apply_changes_to_file_ranges(&changes, &mut file_ranges, args.require_all_files)?;

    Ok(())
}

fn apply_changes_to_file_ranges(
    changes: &HashMap<String, Vec<Vec<String>>>,
    file_ranges: &mut HashMap<String, Vec<(usize, usize)>>,
    require_all_files: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    if require_all_files {
        let mut file_ranges_keys: Vec<&String> = file_ranges.keys().collect();
        file_ranges_keys.sort();
        let mut changes_keys: Vec<&String> = changes.keys().collect();
        changes_keys.sort();
        if file_ranges_keys != changes_keys {
            return Err(Box::new(std::io::Error::other(format!(
                "File ranges keys do not match changes keys. Expected: {changes_keys:?}, Got: {file_ranges_keys:?}"
            ))));
        }
    }

    // When require_all_files is false, we only check that all blocks
    // in changes are ranges present in file_ranges
    for (file_path, blocks) in changes.iter() {
        let ranges = file_ranges.get(file_path).unwrap();
        assert_eq!(ranges.len(), blocks.len());
    }

    for (file_path, file_changes) in changes {
        if let Some(ranges) = file_ranges.get(file_path) {
            // Read the original file
            let content = std::fs::read_to_string(file_path)?;
            let mut lines: Vec<String> = content.lines().map(|s| s.to_string()).collect();

            // Process ranges in reverse order to maintain correct indices
            for (i, range) in ranges.iter().enumerate().rev() {
                let start = range.0 - 1; // Convert to 0-based indexing
                let end = range.1; // End is exclusive in our range

                // Replace the lines in the file
                if let Some(block) = file_changes.get(i) {
                    // Replace the range with new content
                    lines.splice(start..end, block.clone().into_iter());
                }
            }

            // Write back the modified content
            std::fs::write(file_path, lines.join("\n") + "\n")?;
        }
    }

    Ok(())
}

fn parse_file_ranges<'a>(
    lines: impl Iterator<Item = &'a str>,
    context_separator: &str,
) -> HashMap<String, Vec<(usize, usize)>> {
    let mut file_ranges: HashMap<String, Vec<(usize, usize)>> = HashMap::new();
    let mut current_file: Option<String> = None;
    let mut current_range: Option<(usize, usize)> = None;
    let mut prev_line_empty = true;

    for line in lines {
        if line.is_empty() {
            prev_line_empty = true;
            continue;
        }

        if line == context_separator {
            let current = current_file.as_ref().unwrap();
            if let Some(range) = current_range {
                file_ranges.entry(current.clone()).or_default().push(range);
            }
            current_range = None;
            prev_line_empty = false;
            continue;
        }

        // Check if line is a file path
        if !line.chars().next().is_some_and(|c| c.is_ascii_digit()) {
            assert!(prev_line_empty);
            let line = dedup_slashes(line);
            if Path::new(&line).exists() {
                if let Some(ref current) = current_file {
                    if current != &line {
                        if let Some(range) = current_range {
                            file_ranges.entry(current.clone()).or_default().push(range);
                        }
                        current_file = Some(line.to_string());
                    }
                } else {
                    current_file = Some(line.to_string());
                }
                current_range = None;
                prev_line_empty = false;
                continue;
            }
        }

        // Check if line matches the format: linenumber:code
        let mut parts = line.splitn(2, ':');
        if let Some(line_num_str) = parts.next()
            && let Ok(line_num) = line_num_str.parse::<usize>()
        {
            assert!(current_file.is_some());
            // Same file, extend range
            if let Some(ref mut range) = current_range {
                range.1 = line_num;
            } else {
                current_range = Some((line_num, line_num));
            }
        }
        prev_line_empty = false;
    }

    if let Some(ref current) = current_file {
        assert!(!prev_line_empty);
        if let Some(range) = current_range {
            file_ranges.entry(current.clone()).or_default().push(range);
        }
    }

    file_ranges
}

type FileChangesResult = Result<HashMap<String, Vec<Vec<String>>>, Box<dyn std::error::Error>>;

fn parse_modified_file(
    reader: BufReader<File>,
    file_ranges: &HashMap<String, Vec<(usize, usize)>>,
    context_separator: &str,
) -> FileChangesResult {
    let mut changes: HashMap<String, Vec<Vec<String>>> = HashMap::new();
    let mut current_file = String::new();
    let mut current_lines: Vec<String> = Vec::new();
    let mut current_block: Vec<Vec<String>> = Vec::new();
    let mut prev_line_empty = false;
    let mut pprev_line_separator = false;

    for line in reader.lines() {
        let line = line?;
        if line == context_separator {
            assert!(!current_file.is_empty());
            assert!(!current_lines.is_empty());

            current_block.push(current_lines.clone());
            current_lines.clear();

            prev_line_empty = false;
            pprev_line_separator = true;
            continue;
        }
        let normalized_line = dedup_slashes(&line);
        if file_ranges.contains_key(&normalized_line) {
            if !current_file.is_empty() {
                assert!(prev_line_empty);
                assert!(pprev_line_separator);
                assert!(!current_lines.is_empty());

                changes.insert(current_file.clone(), current_block.clone());
                current_block.clear();
            } else {
                assert!(!prev_line_empty);
                assert!(!pprev_line_separator);
            }
            current_file = normalized_line.to_string();
            current_lines.clear();
            prev_line_empty = false;
        } else {
            prev_line_empty = line.is_empty();
            current_lines.push(line);
        }
        if !prev_line_empty {
            pprev_line_separator = false;
        }
    }

    if !current_file.is_empty() {
        assert!(current_lines.is_empty());
        assert!(!prev_line_empty);
        assert!(pprev_line_separator);
        changes.insert(current_file.clone(), current_block.clone());
    }

    Ok(changes)
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_ripgrep_edit_search() {
        let output = std::process::Command::new("cargo")
            .args(&[
                "run",
                "--",
                "--regexp",
                "context_separator",
                "--path",
                "src",
                "--editor",
                "cat",
                "--context=1",
            ])
            .output()
            .expect("Failed to execute ripgrep-edit");

        let output_str = String::from_utf8_lossy(&output.stdout);
        let error_str = String::from_utf8_lossy(&output.stderr);

        // Ensure ripgrep-edit ran successfully
        assert!(
            output.status.success(),
            "Command failed with: {}",
            error_str
        );

        // Check that the output contains the expected lines
        assert!(output_str.contains("src/main.rs"));
        assert!(output_str.contains("context_separa"));
        assert!(error_str.contains("File was not modified"));
    }
}

// Local Variables:
// rust-format-on-save: t
// End:
