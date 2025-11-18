// SPDX-License-Identifier: GPL-3.0-or-later OR AGPL-3.0-or-later
// Copyright (C) 2025  Red Hat, Inc.

use clap::Parser;

#[derive(Parser, Debug, Default)]
#[clap(version, about)]
pub struct Args {
    /// Regex pattern to search for
    #[clap(short = 'e', long, allow_hyphen_values = true)]
    regexp: String,

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

    /// Require all files in the rg output to be present in the modified file
    #[clap(long, default_value_t = false)]
    require_all_files: bool,

    /// context_separator character
    #[clap(
        long,
        default_value = "-- DO NOT DELETE THIS SEPARATOR",
        allow_hyphen_values = true
    )]
    context_separator: String,

    /// filename_prefix character
    #[clap(long, default_value = "-- /", allow_hyphen_values = true)]
    filename_prefix: String,

    /// Smart Case search
    #[clap(short = 'S', long = "smart-case")]
    smart_case: bool,

    /// Ignore case
    #[clap(short = 'i', long = "ignore-case")]
    ignore_case: bool,

    /// Word regexp
    #[clap(short = 'w', long = "word-regexp")]
    word_regexp: bool,

    /// Multiline search
    #[clap(short = 'U', long = "multiline")]
    multiline: bool,

    /// Multiline dotall search
    #[clap(long = "multiline-dotall")]
    multiline_dotall: bool,

    /// Sort results
    #[clap(long, default_value = "none", value_enum)]
    sort: SortBy,

    /// Sort results in reverse
    #[clap(long, default_value = "none", value_enum)]
    sortr: SortBy,

    /// Paths to search in
    #[clap(num_args(0..))]
    paths: Vec<String>,
}

#[derive(Clone, Debug, Default, clap::ValueEnum, PartialEq)]
pub enum SortBy {
    #[default]
    None,
    Path,
    Modified,
    Accessed,
    Created,
}

// Local Variables:
// rust-format-on-save: t
// End:
