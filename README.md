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
> ![ripgrep-edit `"quote_char_in_Range"` commit](https://gitlab.com/aarcange/ripgrep-edit-assets/-/raw/main/demo-quote_char_in_range.webm)
> ![ripgrep-edit `"deduplicate the dedup_by"` commit](https://gitlab.com/aarcange/ripgrep-edit-assets/-/raw/main/demo-dedup_slashes.webm)

## Build

```bash
git clone https://gitlab.com/aarcange/ripgrep-edit.git
cd ripgrep-edit
cargo build --release
```

## Distro Packages

### Fedora Copr
A Fedora Copr package is available:
```bash
sudo dnf copr enable vittyvk/ripgrep-edit
sudo dnf install ripgrep-edit
```

The Copr is built for Fedora rawhide / 43 / 42.

## Options

- `-e, --regexp <REGEXP>`: Regex pattern to search for  
- `-E, --editor <EDITOR>`: Editor command (e.g., `vim`, `emacs`)  
- `-C, --context <N>`: Number of context lines  
- `-A, --after-context <N>`: Lines of context to show after each match  
- `-B, --before-context <N>`: Lines of context to show before each match  
- `--require-all-files`: Require all files in ripgrep output to be present in the edited file  
- `--context-separator <SEPARATOR>`: Separator string between snippets  
- `--filename-prefix <PREFIX>`: Prefix for filename lines  
- `-S, --smart-case`: Enable smart case search  
- `-i, --ignore-case`: Ignore case distinctions  
- `-w, --word-regexp`: Force pattern to match only whole words  
- `-U, --multiline`: Enable multiline matching  
- `--multiline-dotall`: Enable dotall mode for multiline matching  
- `--sort <SORT>`: Sort results by path, modified, accessed, or created time  
- `--sortr <SORT>`: Sort results in reverse order by path, modified, accessed, or created time  
- `--follow`: Follow symlinks  
- `--dump-on-error`: Dump processed tempfile to stderr on error
- `--gbnf`: Generate dynamic GBNF grammar file  
- `--gbnf-control-lines`: Max number of GBNF control lines added as extra context  

## Examples

```bash
rg-edit [OPTIONS] --regexp <REGEXP> --editor <EDITOR> [PATHS...]
rg-edit -e "function\s*\(" -E vim src/
rg-edit -e "function\s*\(" -E "emacsclient" -C 8 src/
rg-edit -E vim -U -e '(?s)^<<<<<<<+ .*?^>>>>>>>+ ' # resolve git conflicts
```

## Why It Was Built

Large codebases challenge LLMs due to context limits. Traditional vibe coding tools require reading snippets of multiple large files through the LLM agentic workflow.

`ripgrep-edit` solves this by:
- 🔄 **Linearizing** code snippets from multiple files
- 📌 **Preserving context** around each match
- 🧠 **Enabling precise edits** via LLMs or editors
- 🔄 **Reapplying changes to original files**

`ripgrep-edit` lets the programmer define the exact context needed, presenting it in a structured, linear format optimized for the LLM's key-value input. The resulting gain in efficiency enables real-time visibility into what the LLM is generating — allowing to interrupt, refine input, adjust directives, and eliminate unnecessary context, computation, and cost.

The goal is to optimize the LLM's most demanding task — writing code — by minimizing perplexity and maximizing efficiency.

## How to use

This works best with tools like [`gptel-rewrite`](https://github.com/karthink/gptel), where extra context can be included for more complex refactorings.

To be effective, `ripgrep-edit` requires a well-crafted prompt:

```
You are a careful programmer. Rewrite cross-file snippets.
Rewrite everything exactly the same except: the required change.
Keep the filenames at the start of the files.
Keep the separators at the end of the snippets.
Do not delete the filenames and the separators.
Do not add markdown fences.
Do not ask clarification.
```

## rg-edit.el

- Provides `rg-edit`, `rg-edit-git`, `rg-edit-git-conflicts` functions
- `rg-edit-git` **C-c r**
- `rg-edit-git-conflicts` **C-u C-c r**

```elisp
(add-to-list 'load-path "~/.../ripgrep-edit/emacs/")
(use-package rg-edit
  :ensure nil
  :config
  (setq-default rg-edit-auto-mark-whole-buffer t)
  (setq-default rg-edit-executable "~/.../ripgrep-edit/target/release/rg-edit"))
```

## GBNF Grammar Support

ripgrep-edit integrates with [GBNF](https://github.com/ggml-org/llama.cpp/blob/master/grammars/README.md) to constrain LLM outputs when using llama.cpp as the backend. When the `--gbnf` flag is used, a `*.gbnf` grammar file is generated in the same directory as the temporary rg-edit buffer. The `rg-edit.el` plugin automatically detects this file and injects its contents into the JSON request as the `grammar` field when the gptel backend supports the `gbnf` capability.

```
(setq-default
 gptel-model `test
 gptel-backend (gptel-make-openai "test"
		 :stream t
		 :protocol "http"
		 :host "localhost:8811"
		 :models '((test :capabilities (gbnf))))
```

The GBNF grammar enforces deterministic structure around the metadata markers of the rg-edit format. The LLM retains the ability to drop irrelevant files or reorder the file sequence, but the filename and its prefix are enforced to have no typos. Once inside the snippets, the GBNF grammar emits the "before context" control line (if available) and then grants the LLM free reign to modify snippet content until the LLM emits the "after context" control line or the separator. At that point, the GBNF grammar regains control, emits the separator (if not already emitted), and advances to the next snippet.

When the GBNF grammar is enabled during inference, a notification appears in the `*rg-edit*` buffer.

> ![ripgrep-edit `"max_line_1 with GBNF"` commit](https://gitlab.com/aarcange/ripgrep-edit-assets/-/raw/main/demo-max_line_1-GBNF.webm)
> ![ripgrep-edit `"usage limit with GBNF"` commit](https://gitlab.com/aarcange/ripgrep-edit-assets/-/raw/main/demo-usage_limit-GBNF.webm)

## Why Not wgrep?

- Emacs' [`wgrep`](https://github.com/mhayashi1120/Emacs-wgrep) offers similar functionality but relies on interactive edits, such as `C-c C-d` to delete a line, which are incompatible with the full rewrites required in LLM-driven workflows.

- Unlike wgrep, `ripgrep-edit` is editor-agnostic and LLM-agnostic: any editor can be used, making it ideal for diverse environments, or simply to avoid opening multiple files with vim.

## License

[![License: GPL-3.0-or-later](https://img.shields.io/badge/License-GPL--3.0--or--later-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.html)
[![License: AGPL-3.0-or-later](https://img.shields.io/badge/License-AGPL--3.0--or--later-blue.svg)](https://www.gnu.org/licenses/agpl-3.0.html)
