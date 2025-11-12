# ripgrep-edit - Razor Coding Tool

ripgrep-edit enables *razor coding* by allowing LLM workflows to operate with razor-thin context, input and output.

## How it works

- 🔍 Search using `ripgrep` with configurable context lines  
- 📄 Present matches in a clean, linear, easy-to-read format  
- ✏️ Edit all matched code snippets in a single temporary file  
- 💾 Apply changes back to original files  
- 🛠️ Works with any editor via shell command  

## Demo

> ![ripgrep-edit `"kernel arg_lock from spinlock to mutex"` commit](https://gitlab.com/aarcange/ripgrep-edit-assets/-/raw/main/demo-kernel-arg_lock.webm)
> ![ripgrep-edit `"deduplicate the dedup_by"` commit](https://gitlab.com/aarcange/ripgrep-edit-assets/-/raw/main/demo-dedup_slashes.webm)

## Build

```bash
cargo build --release
```

## Options

- `-e, --regexp <REGEXP>`: Regex pattern to search for  
- `-p, --path <PATH>`: Directory or file to search in  
- `-E, --editor <EDITOR>`: Editor command (e.g., `vim`, `emacs`)  
- `-C, --context <N>`: Number of context lines (default: 5)  
- `-A, --after-context <N>`: Lines of context to show after each match  
- `-B, --before-context <N>`: Lines of context to show before each match  
- `--require-all-files`: Require all files in ripgrep output to be present in the edited file  
- `--context-separator <SEPARATOR>`: Separator string between snippets  
- `--filename-prefix <PREFIX>`: Prefix for filename lines (default: "-- /")  
- `-S, --smart-case`: Enable smart case search  
- `-i, --ignore-case`: Ignore case distinctions  
- `-w, --word-regexp`: Force pattern to match only whole words  
- `-U, --multiline`: Enable multiline matching  
- `--multiline-dotall`: Enable dotall mode for multiline matching

## Examples

```bash
ripgrep-edit [OPTIONS] --regexp <REGEXP> --path <PATH> --editor <EDITOR>
ripgrep-edit -e "function\s*\(" -p src/ -E vim
ripgrep-edit -e "function\s*\(" -p src/ -E "emacsclient" -C 8
ripgrep-edit -E vim -U -e '(?s)^<<<<<<<+ .*?^>>>>>>>+ ' # resolve git conflicts
```

## Why It Was Built

Large codebases challenge LLMs due to context limits. Traditional vibe coding tools require reading snippets of multiple large files through the LLM agentic workflow.

`ripgrep-edit` solves this by:
- 🔄 **Linearizing** code snippets from multiple files
- 📌 **Preserving context** around each match
- 🧠 **Enabling precise edits** via LLMs or editors
- 🔄 **Reapplying changes to original files**

`ripgrep-edit` lets the programmer define the exact context needed, presenting it in a structured, linear format optimized for the LLM’s key-value input. The resulting gain in efficiency enables real-time visibility into what the LLM is generating — allowing to interrupt, refine input, adjust directives, and eliminate unnecessary context, computation, and cost.

The goal is to optimize the LLM’s most demanding task — writing code — by minimizing perplexity and maximizing efficiency.

## How to use

This works best with tools like [`gptel-rewrite`](https://github.com/karthink/gptel), where extra context can be included for more complex refactorings.

To be effective, `ripgrep-edit` requires a well-crafted prompt:

```
You are a careful programmer. Rewrite cross-file snippnets.
Rewrite everything exactly the same except: the required change.
Keep the filenames at the start of the files.
Keep the separators at the end of the snippnets.
Do not change the line after the filenames and before the separators.
Do not add markdown fences.
Do not ask clarification.
```

## ripgrep-edit.el

- Provides `ripgrep-edit` and `ripgrep-edit-git` functions

```elisp
(load-file "~/.../ripgrep-edit/emacs/ripgrep-edit.el")
(setq-default ripgrep-edit-executable "~/.../ripgrep-edit/target/release/ripgrep-edit")
```

## Why Not wgrep?

- Emacs’ [`wgrep`](https://github.com/mhayashi1120/Emacs-wgrep) offers similar functionality but relies on interactive edits, such as `C-c C-d` to delete a line, which are incompatible with the full rewrites required in LLM-driven workflows.

- Unlike wgrep, `ripgrep-edit` is editor-agnostic and LLM-agnostic: any editor can be used, making it ideal for diverse environments, or simply to avoid opening multiple files with vim.

## License

[![License: GPL-3.0-or-later](https://img.shields.io/badge/License-GPL--3.0--or--later-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.html)
[![License: AGPL-3.0-or-later](https://img.shields.io/badge/License-AGPL--3.0--or--later-blue.svg)](https://www.gnu.org/licenses/agpl-3.0.html)
