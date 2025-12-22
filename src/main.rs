// SPDX-License-Identifier: GPL-3.0-or-later OR AGPL-3.0-or-later
// Copyright (C) 2025  Red Hat, Inc.

use anyhow::Result;
use gbnf::generate_gbnf_file;
use std::collections::{HashMap, HashSet};
use std::fs::{self, File};
use std::io::{BufRead, BufReader, Write};
use std::path::Path;
use std::process::Command;
use std::time::Duration;
use std::time::SystemTime;

mod gbnf;

include!("args.rs");

impl Args {
    pub fn validate(&self) -> Result<()> {
        if self.sort != SortBy::None && self.sortr != SortBy::None {
            return Err(anyhow::anyhow!("Cannot specify both --sort and --sortr"));
        }
        Ok(())
    }
}

#[derive(Debug)]
struct FileRange {
    start: usize,
    end: usize,
    lines: Vec<String>,
    before_context: usize,
    after_context: usize,
}

impl FileRange {
    fn new(
        start: usize,
        end: usize,
        lines: Vec<String>,
        before_context: usize,
        after_context: usize,
    ) -> Self {
        Self {
            start,
            end,
            lines,
            before_context,
            after_context,
        }
    }
}

#[derive(Debug)]
struct FileRanges {
    filenames: Vec<String>,
    hash: HashMap<String, Vec<FileRange>>,
    context_separator: String,
    filename_prefix: String,
}

impl FileRanges {
    fn new(rg_output: &str, context_separator: &str, filename_prefix: &str) -> Result<Self> {
        // Parse the output to extract file paths before writing to temp file
        let lines = rg_output.lines().peekable();

        let mut file_ranges: FileRanges = FileRanges {
            filenames: Vec::new(),
            hash: HashMap::new(),
            context_separator: context_separator.to_string(),
            filename_prefix: filename_prefix.to_string(),
        };
        let mut current_file: Option<String> = None;
        let mut current_range: Option<FileRange> = None;
        let mut context_is_before = true;
        let mut prev_line_empty = true;

        for line in lines {
            if line.is_empty() {
                prev_line_empty = true;
                continue;
            }

            // Check if line is a file path
            if prev_line_empty {
                let line = dedup_slashes(line);

                if !Path::new(&line).exists() {
                    anyhow::bail!("File does not exist: {}", line);
                }

                if let Some(ref current) = current_file {
                    if current != &line {
                        if let Some(range) = current_range {
                            file_ranges.add(current, range);
                        }
                        current_file = Some(line.to_string());
                    }
                } else {
                    current_file = Some(line.to_string());
                }
                current_range = None;
                context_is_before = true;
                prev_line_empty = false;
                continue;
            }

            if current_file.is_none() {
                anyhow::bail!("No current file set when processing line: {}", line);
            }

            if line == context_separator {
                let current = current_file.as_ref().unwrap();
                if let Some(range) = current_range {
                    file_ranges.add(current, range);
                }
                current_range = None;
                context_is_before = true;
                prev_line_empty = false;
                continue;
            }

            // Check if line matches the format: linenumber:code
            let line_code = FileRanges::parse_line(line, RG_MATCH_SEPARATOR);
            let ((line_num, line), is_context) = if line_code.is_err() {
                (FileRanges::parse_line(line, RG_CONTEXT_SEPARATOR)?, true)
            } else {
                (line_code?, false)
            };
            if line == context_separator {
                anyhow::bail!("context_separator found in file: {}", current_file.unwrap());
            }
            // Same file, extend range
            if let Some(ref mut range) = current_range {
                range.end = line_num;
                range.lines.push(line);
                if is_context {
                    if context_is_before {
                        range.before_context += 1;
                    } else {
                        range.after_context += 1;
                    }
                } else {
                    context_is_before = false;
                    range.after_context = 0;
                }
            } else {
                assert!(context_is_before);
                let before_context = if is_context { 1 } else { 0 };
                current_range = Some(FileRange::new(
                    line_num,
                    line_num,
                    vec![line],
                    before_context,
                    0,
                ));
                if !is_context {
                    context_is_before = false;
                }
            }
            prev_line_empty = false;
        }

        if let Some(ref current) = current_file {
            if prev_line_empty {
                anyhow::bail!(
                    "Unexpected empty line after file processing for file: {}",
                    current
                );
            }
            if let Some(range) = current_range {
                file_ranges.add(current, range);
            }
        }

        file_ranges.validate()?;
        Ok(file_ranges)
    }

    fn contains_key(&self, key: &str) -> bool {
        self.hash.contains_key(key)
    }

    fn parse_line(line: &str, separator: &str) -> Result<(usize, String)> {
        let mut parts = line.splitn(2, separator);
        if let Some(line_num_str) = parts.next()
            && let Ok(line_num) = line_num_str.parse::<usize>()
        {
            let code = parts.next().unwrap().to_string();
            Ok((line_num, code))
        } else {
            Err(anyhow::anyhow!(
                "Failed to parse line number from line: {}",
                line
            ))
        }
    }

    fn add(&mut self, filename: &str, value: FileRange) {
        if !self.contains_key(filename) {
            self.filenames.push(filename.to_string());
        }
        self.hash
            .entry(filename.to_string())
            .or_default()
            .push(value);
    }

    fn iter(&self) -> impl Iterator<Item = (&String, &Vec<FileRange>)> {
        self.filenames
            .iter()
            .filter_map(move |filename| self.hash.get(filename).map(|ranges| (filename, ranges)))
    }

    fn validate(&self) -> Result<()> {
        // Check all FileRange and enforce the range.start..range.end matches lines.len()
        for (filename, ranges) in &self.hash {
            if ranges.is_empty() {
                anyhow::bail!("Empty ranges for file: {}", filename);
            }
            for range in ranges {
                if range.lines.is_empty() {
                    anyhow::bail!("Empty lines in range for file {}", filename);
                }
                let expected_lines = range.end - range.start + 1;
                if expected_lines != range.lines.len() {
                    anyhow::bail!(
                        "Mismatch in line count for file {} at range {:?}: expected {}, got {}",
                        filename,
                        range,
                        expected_lines,
                        range.lines.len()
                    );
                }
            }
        }

        // Use a HashSet and verify there's no dup in the filenames
        // and that the filenames HashSet is equal to the HashSet
        // created from hash.keys()
        let filenames_set: HashSet<&String> = self.filenames.iter().collect();
        let hash_keys_set: HashSet<&String> = self.hash.keys().collect();

        if filenames_set.len() != self.filenames.len() {
            anyhow::bail!("Duplicate filenames found in filenames list");
        }

        if filenames_set != hash_keys_set {
            anyhow::bail!("Mismatch between filenames list and hash keys");
        }
        Ok(())
    }

    fn write(&self, file: &mut std::fs::File) -> Result<()> {
        writeln!(file, "```")?;
        for (i, (filename, ranges)) in self.iter().enumerate() {
            if i > 0 {
                writeln!(file)?;
            }
            writeln!(file, "{}{}", self.filename_prefix, filename)?;
            for range in ranges {
                writeln!(file, "{}", range.lines.join("\n"))?;
                writeln!(file, "{}", self.context_separator)?;
            }
        }
        writeln!(file, "```")?;
        file.flush()?;
        Ok(())
    }
}

type FileChanges = HashMap<String, Vec<Vec<String>>>;

const RG_MATCH_SEPARATOR: &str = ":";
const RG_CONTEXT_SEPARATOR: &str = ";";

fn dedup_slashes(line: &str) -> String {
    let mut chars = line.chars().collect::<Vec<char>>();
    chars.dedup_by(|a, b| *a == '/' && *a == *b);
    chars.iter().collect::<String>()
}

fn main() -> Result<()> {
    let args = Args::parse();
    args.validate()?;

    // Run rg with context lines
    let context_separator = &args.context_separator;
    let filename_prefix = &args.filename_prefix;

    let mut before_context = args.context.max(args.before_context) as usize;
    let mut after_context = args.context.max(args.after_context) as usize;
    if args.gbnf {
        before_context += args.gbnf_control_lines as usize;
        after_context += args.gbnf_control_lines as usize;
    }
    let (before_context, after_context) = (before_context, after_context);

    let mut rg_cmd = Command::new("rg");
    rg_cmd
        .arg("-n")
        .arg("-H")
        .arg(format!("-A{}", after_context))
        .arg(format!("-B{}", before_context))
        .arg("--heading")
        .arg("--color=never")
        .arg(format!(
            "--field-context-separator={}",
            RG_CONTEXT_SEPARATOR
        ))
        .arg(format!("--field-match-separator={}", RG_MATCH_SEPARATOR))
        .arg(format!("--context-separator={context_separator}"))
        .arg("-e")
        .arg(&args.regexp);

    if args.smart_case {
        rg_cmd.arg("-S");
    }
    if args.ignore_case {
        rg_cmd.arg("-i");
    }
    if args.word_regexp {
        rg_cmd.arg("-w");
    }
    if args.multiline {
        rg_cmd.arg("-U");
    }
    if args.multiline_dotall {
        rg_cmd.arg("--multiline-dotall");
    }
    if args.sort != SortBy::None {
        rg_cmd.arg("--sort");
        rg_cmd.arg(match args.sort {
            SortBy::None => unreachable!(),
            SortBy::Path => "path",
            SortBy::Modified => "modified",
            SortBy::Accessed => "accessed",
            SortBy::Created => "created",
        });
    }
    if args.sortr != SortBy::None {
        rg_cmd.arg("--sortr");
        rg_cmd.arg(match args.sortr {
            SortBy::None => unreachable!(),
            SortBy::Path => "path",
            SortBy::Modified => "modified",
            SortBy::Accessed => "accessed",
            SortBy::Created => "created",
        });
    }
    for path in &args.paths {
        rg_cmd.arg(path);
    }

    let output = rg_cmd.output();
    match output {
        Ok(_) => {}
        Err(e) => {
            if e.kind() == std::io::ErrorKind::NotFound {
                anyhow::bail!("Error: rg is not installed.");
            } else {
                anyhow::bail!("Failed to execute rg: {e}");
            }
        }
    }
    let rg_output = String::from_utf8_lossy(&output.unwrap().stdout).into_owned();
    if rg_output.trim().is_empty() {
        eprintln!("No results found.");
        std::process::exit(0);
    }

    let mut file_ranges = FileRanges::new(&rg_output, context_separator, filename_prefix)?;

    let mut temp_file = tempfile::Builder::new()
        .prefix("rg-edit-")
        .suffix(".rg-edit")
        .tempfile()?;

    let _gbnf;
    if args.gbnf {
        _gbnf = generate_gbnf_file(&mut file_ranges, temp_file.path(), &args)?;
    }
    let file_ranges = file_ranges;

    let temp_path = temp_file.path().to_str().unwrap().to_string();
    let file = temp_file.as_file_mut();
    file_ranges.write(file)?;

    // Set the temp_path modification time to 1 day before the current time
    let before_edit = SystemTime::now() - Duration::from_secs(24 * 60 * 60);
    file.set_modified(before_edit)?;

    // Open with editor
    let mut editor_cmd = Command::new("sh");
    editor_cmd
        .arg("-c")
        .arg(format!("{} {}", args.editor, temp_path));

    let editor_status = editor_cmd.status()?;
    if !editor_status.success() {
        anyhow::bail!("Editor failed with status: {}", editor_status);
    }

    // Get modification time after editor
    let after_edit = fs::metadata(&temp_path)?.modified()?;

    // Check if the file was modified
    if after_edit <= before_edit {
        eprintln!("File was not modified by the editor. Exiting without changes.");
        return Ok(());
    }

    // Read modified file
    let file = File::open(temp_path)?;
    let reader = BufReader::new(file);

    let mut processed_lines: Vec<String> = Vec::new();
    if let Err(e) = parse_modified_file(
        reader,
        &file_ranges,
        context_separator,
        filename_prefix,
        &args,
        &mut processed_lines,
    ) {
        if args.dump_on_error {
            print_processed_lines(&processed_lines);
        }
        return Err(e);
    }

    Ok(())
}

fn apply_changes_to_file_ranges(
    changes: &FileChanges,
    file_ranges: &FileRanges,
    args: &Args,
) -> Result<()> {
    let file_ranges_keys: HashSet<&String> = file_ranges.filenames.iter().collect();
    let changes_keys: HashSet<&String> = changes.keys().collect();
    assert!(
        changes_keys.is_subset(&file_ranges_keys),
        "changes contain files not found in file_ranges"
    );
    if args.require_all_files {
        let missing_files: Vec<&String> = file_ranges_keys
            .difference(&changes_keys)
            .copied()
            .collect();
        if !missing_files.is_empty() {
            return Err(anyhow::anyhow!("Missing files: {:?}", missing_files));
        }
    }

    // When require_all_files is false, we only check that all snippets
    // in changes are ranges present in file_ranges
    for filename in changes_keys.iter() {
        assert!(validate_changes_vs_ranges(changes, file_ranges, filename).is_ok());
    }

    let mut changed_files = false;

    for (file_path, file_changes) in changes {
        if let Some(ranges) = file_ranges.hash.get(file_path) {
            // Read the original file
            let content = std::fs::read_to_string(file_path)?;
            let mut lines: Vec<String> = content.lines().map(|s| s.to_string()).collect();

            // Check if any changes are actually different from original
            let mut has_changes = false;

            // Process ranges in reverse order to maintain correct indices
            for (i, range) in ranges.iter().enumerate().rev() {
                let start = range.start - 1; // Convert to 0-based indexing
                let end = range.end; // End is exclusive in our range

                // Replace the lines in the file
                if let Some(snippet) = file_changes.get(i) {
                    let original_snippet: Vec<String> = lines[start..end].to_vec();
                    if *snippet != original_snippet {
                        // Replace the range with new content
                        lines.splice(start..end, snippet.clone().into_iter());
                        has_changes = true;
                        changed_files = true;
                    }
                }
            }

            if has_changes {
                // Write back the modified content
                std::fs::write(file_path, lines.join("\n") + "\n")?;
            }
        }
    }

    if !changed_files {
        eprintln!("No changes detected in any file.");
    }

    Ok(())
}

fn parse_modified_file(
    reader: BufReader<File>,
    file_ranges: &FileRanges,
    context_separator: &str,
    filename_prefix: &str,
    args: &Args,
    processed_lines: &mut Vec<String>,
) -> Result<()> {
    let mut changes: FileChanges = HashMap::new();
    let mut current_file = String::new();
    let mut current_lines: Vec<String> = Vec::new();
    let mut current_snippet: Vec<Vec<String>> = Vec::new();
    let mut prev_line_empty = false;
    let mut pprev_line_separator = false;
    let mut first = true;

    for line in reader.lines() {
        let line = line?;
        processed_lines.push(line.clone());

        if first && line == "```" {
            first = false;
            continue;
        }

        if line == context_separator {
            if current_file.is_empty() {
                return Err(anyhow::anyhow!(
                    "Context separator found without active file"
                ));
            }
            if current_lines.is_empty() {
                return Err(anyhow::anyhow!(
                    "Empty snippet found in file: {}",
                    current_file
                ));
            }

            current_snippet.push(current_lines.clone());
            current_lines.clear();

            prev_line_empty = false;
            pprev_line_separator = true;
            continue;
        }
        let normalized_line = line.strip_prefix(filename_prefix).map(dedup_slashes);
        if let Some(normalized_line) = normalized_line
            && file_ranges.contains_key(&normalized_line)
        {
            if !current_file.is_empty() {
                if !pprev_line_separator {
                    return Err(anyhow::anyhow!(
                        "Missing separator before file: {}",
                        &normalized_line
                    ));
                }
                if !prev_line_empty {
                    eprintln!("Warning: Line not empty before file: {}", &normalized_line);
                } else {
                    assert!(!current_lines.is_empty());
                }

                if changes
                    .insert(current_file.clone(), current_snippet.clone())
                    .is_some()
                {
                    return Err(anyhow::anyhow!("Duplicate file found: {}", current_file));
                }
                validate_changes_vs_ranges(&changes, file_ranges, &current_file)?;
                current_snippet.clear();
            } else {
                assert!(!pprev_line_separator);
                if !current_lines.is_empty() {
                    eprintln!(
                        "Warning: trailing lines before first file: {}",
                        &normalized_line
                    );
                } else {
                    assert!(!prev_line_empty);
                }
            }
            current_file = normalized_line.to_string();
            current_lines.clear();
            prev_line_empty = false;
        } else {
            prev_line_empty = line.trim().is_empty();
            current_lines.push(line);
        }
        if !prev_line_empty {
            pprev_line_separator = false;
        }
    }

    if !current_file.is_empty() {
        if current_lines == vec!["```".to_string()] {
            current_lines.pop();
            pprev_line_separator = true;
        }
        if !current_lines.is_empty() {
            eprintln!("Warning: Trailing lines after last file: {}", current_file);
        } else {
            assert!(!prev_line_empty);
            assert!(pprev_line_separator);
        }
        if changes
            .insert(current_file.clone(), current_snippet.clone())
            .is_some()
        {
            return Err(anyhow::anyhow!("Duplicate file found: {}", current_file));
        }
        validate_changes_vs_ranges(&changes, file_ranges, &current_file)?;
    }

    apply_changes_to_file_ranges(&changes, file_ranges, args)
}

fn validate_changes_vs_ranges(
    changes: &FileChanges,
    file_ranges: &FileRanges,
    current_file: &str,
) -> Result<()> {
    let changes_len = changes.get(current_file).unwrap().len();
    let ranges_len = file_ranges.hash.get(current_file).unwrap().len();
    if changes_len != ranges_len {
        return Err(anyhow::anyhow!(
            "Mismatch between changes and file ranges in file {}: expected {} snippets, found {}",
            current_file,
            ranges_len,
            changes_len
        ));
    }
    Ok(())
}

fn print_processed_lines(lines: &[String]) {
    for line in lines {
        eprintln!("{}", line);
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_rg_edit_search() {
        let output = std::process::Command::new("cargo")
            .args(&[
                "run",
                "--",
                "--regexp",
                "context_separator",
                "src",
                "--editor",
                "cat",
                "--context=1",
                "--filename-prefix",
                "-- TEST: ",
            ])
            .output()
            .expect("Failed to execute rg-edit");

        let output_str = String::from_utf8_lossy(&output.stdout);
        let error_str = String::from_utf8_lossy(&output.stderr);

        // Ensure rg-edit ran successfully
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
