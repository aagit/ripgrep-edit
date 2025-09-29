# ripgrep-edit - Razor Coding Tool

ripgrep-edit enables *razor coding* by allowing LLM workflows to operate with razor-thin context, input and output.

## How it works

- 🔍 Search using `ripgrep` with configurable context lines  
- 📄 Present matches in a clean, linear, easy-to-read format  
- ✏️ Edit all matched file sections in a single temporary file  
- 💾 Apply changes back to original files  
- 🛠️ Works with any editor via shell command  

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
- `--context-separator <CHAR>`: Separator string between sections (default: `DEL`)

## Examples

```bash
./target/release/ripgrep-edit [OPTIONS] --regexp <REGEXP> --path <PATH> --editor <EDITOR>
./target/release/ripgrep-edit -e "function\s*\(" -p src/ -E vim
./target/release/ripgrep-edit -e "function\s*\(" -p src/ -E "emacsclient" -C 8
```

## Why It Was Built

Large codebases challenge LLMs due to context limits. Traditional vibe coding tools require reading snippets of multiple large files through the LLM agentic workflow.

`ripgrep-edit` solves this by:
- 🔄 **Linearizing** code sections from multiple files
- 📌 **Preserving context** around each match
- 🧠 **Enabling precise edits** via LLMs or editors
- 🔄 **Reapplying changes to original files**

`ripgrep-edit` lets the programmer define the exact context needed, presenting it in a structured, linear format optimized for the LLM’s key-value input. The resulting gain in efficiency enables real-time visibility into what the LLM is generating — allowing to interrupt, refine input, adjust directives, and eliminate unnecessary context, computation, and cost.

The goal is to optimize the LLM’s most demanding task — writing code — by minimizing perplexity and maximizing efficiency.

This works best with tools like [`gptel-rewrite`](https://github.com/karthink/gptel), where extra context can be included for more complex refactorings.

To be effective, `ripgrep-edit` requires a well-crafted prompt (like those used in `gptel-rewrite`) to guide the LLM.

## Model Compatibility

An example of a open weight model that appears to work with this workflow without requiring fine tuning:  
**Qwen3-Coder-30B-A3B-Instruct**

## gptel-rewrite Directive Examples

- `"This is a cross-file edit. Rewrite everything exactly the same, except: ..."`
- `"This is a cross-file edit. Each section of the file is delimited by the DEL character. Don't change anything, except: ..."`
- `"This is a cross-file edit. Each section is delimited by DEL. Don't change anything, except: ..."`
- `"This is a cross file edit. Rewrite everything exactly the same, except these 3 instructions: ..."`

## Why Not wgrep?

- Emacs’ [`wgrep`](https://github.com/mhayashi1120/Emacs-wgrep) offers similar functionality but relies on interactive edits, such as `C-c C-d` to delete a line, which are incompatible with the full rewrites required in LLM-driven workflows.

- Unlike wgrep, `ripgrep-edit` is editor-agnostic and LLM-agnostic: any editor can be used, making it ideal for diverse environments, or simply to avoid opening multiple files with vim.

## License

[![License: GPL-3.0-or-later](https://img.shields.io/badge/License-GPL--3.0--or--later-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.html)
[![License: AGPL-3.0-or-later](https://img.shields.io/badge/License-AGPL--3.0--or--later-blue.svg)](https://www.gnu.org/licenses/agpl-3.0.html)
