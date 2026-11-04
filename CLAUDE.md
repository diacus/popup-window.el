# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository

Single-file Emacs Lisp package: `popup-window.el`. It provides the `make-repl-popup` macro, which wraps a REPL-launching function so the REPL runs in a bottom side window, and arranges for the buffer and window to be cleaned up when the REPL process exits.

## Commands

- Byte-compile check (catches syntax/compile warnings): `emacs -Q --batch -L . -f batch-byte-compile popup-window.el`
- Run the ERT test suite: `emacs -Q --batch -L . -l test/popup-window-test.el -f ert-run-tests-batch-and-exit`
- There is no build system or package manifest (no Cask). The repo is currently an MVP.
- Use `-Q` on batch invocations: site-start files in this environment load broken config fragments that interfere with batch Emacs.

## Architecture

`make-repl-popup` (the only definition) works in three steps:

1. Binds a dynamic `display-buffer-alist` so any buffer whose name matches `\*.*\*` is shown via `display-buffer-in-side-window` (bottom side, height `popup-window-height`, a fraction of the frame or a number of lines, defaulting to 0.3), then calls the wrapped REPL function.
2. Identifies the REPL buffer by diffing `(buffer-list)` before/after the call — this relies on the REPL function creating exactly one new buffer as its *first* new buffer.
3. Attaches a process sentinel that kills the buffer (closing the window) when the process event string matches `status|exited|finished`.

Constraints to keep in mind when modifying:
- The `display-buffer-alist` regex matches all `*...*` buffers, not just the REPL's; a REPL that opens auxiliary `*foo*` buffers will also pop them up.
- The REPL is located via `car` of the buffer diff, so ordering of new buffers matters.
- The macro requires `cl-set-difference` at runtime, loaded via `(require 'cl-lib)` in popup-window.el; lexical binding is enabled.

## Testing

`test/popup-window-test.el` (ERT) drives the macro with fake REPL functions that spawn `sleep` processes in a scratch buffer, so behavior is exercised headlessly in batch. Note that `(funcall (make-repl-popup ...))` returns the value of `set-process-sentinel` (or nil), not the REPL buffer — the tests reach the buffer via a variable set inside the fake REPL itself.