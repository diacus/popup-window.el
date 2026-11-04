;;; popup-window.el --- Runs helper tool within popup window -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Diacus Magnuz

;; Author: Diacus Magnuz <diacus.magnuz@gmail.com>
;; Maintainer: Diacus Magnuz <diacus.magnuz@gmail.com>
;; URL: https://github.com/diacus/popup-window.el
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools convenience

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; popup-window.el runs a custom repl inside a temporary popup window
;;
;;; Code:
(require 'cl-lib)

(defgroup popup-window nil
  "Run REPLs inside temporary popup windows."
  :group 'tools)

(defcustom popup-window-height 0.3
  "Height of the popup side window.
A float between 0 and 1 is a fraction of the frame height; an
integer is a fixed number of lines."
  :type '(choice (float :tag "Fraction of frame height" :value 0.3)
                 (integer :tag "Lines"))
  :group 'popup-window)

(defcustom popup-window-side 'bottom
  "Where the popup side window is displayed.
Either `bottom' or `top'.  `popup-window-height' applies either
way, as the height of the popup window itself."
  :type '(radio (const :tag "Bottom of the frame" bottom)
                (const :tag "Top of the frame" top))
  :group 'popup-window)

(defmacro make-repl-popup (repl-function)
  "Expand to a function running REPL-FUNCTION inside a popup window.

The expansion is an interactive lambda taking any number of
arguments.  Invoking it:

1. Binds `display-buffer-alist' so that every buffer the call
   displays whose name matches \\*.*\\* is shown via
   `display-buffer-in-side-window', on the side given by
   `popup-window-side' (bottom by default) with the height given
   by `popup-window-height', and then runs REPL-FUNCTION.

2. Identifies the REPL buffer as the one displayed most recently
   among the buffers newly created by the call.

3. Attaches a sentinel to the REPL process that kills its buffer
   when the process exits, which closes the popup window with it.

Arguments passed to the expansion are forwarded to REPL-FUNCTION
as-is.  When invoked without arguments and REPL-FUNCTION is an
interactive command, it runs through its own interactive form, so
its prompts still work.

The expansion returns the result of setting the process sentinel
\(see `set-process-sentinel'), not the REPL buffer; reach the
buffer through the wrapped function instead."
  (let ((wrapped-name (cond
                       ((symbolp repl-function)
                        (symbol-name repl-function))
                       ((and (consp repl-function)
                             (memq (car repl-function) '(quote function))
                             (symbolp (cadr repl-function)))
                        (symbol-name (cadr repl-function))))))
    `(lambda (&rest args)
       ,(concat
         (if wrapped-name
             (format "Run `%s' inside a temporary side-window popup.\n"
                     wrapped-name)
           "Run the wrapped function inside a temporary side-window popup.\n")
         "
ARGS are forwarded to it as-is.  With no arguments, a wrapped
interactive command runs through its own interactive form, so its
prompts still apply.
Every \\*.*\\* buffer it displays shows up in the side window
chosen by `popup-window-side', with height `popup-window-height'.
The LAST newly displayed buffer is the REPL buffer; a sentinel
kills it -- closing the window -- when its process exits.

Returns the result of `set-process-sentinel', not the REPL buffer.")
       (interactive)
       (let ((repl-fn ,repl-function)
             (display-buffer-alist
              `(("\\*.*\\*"
                 (display-buffer-in-side-window)
                 (side . ,popup-window-side)
                 (window-height . ,popup-window-height))))
             ;; Some REPLs switch with `switch-to-buffer' (multi-vterm
             ;; does), which never consults `display-buffer-alist'
             ;; unless this option is on: without it the popup never
             ;; happens and the buffer takes over the current window.
             (switch-to-buffer-obey-display-actions t)
             (old-buffers (buffer-list)))
         ;; 1. Run the REPL function.  When no arguments were given and
         ;; it is a command (e.g. `term', which prompts for the program
         ;; to run), run it through its own interactive form, so the
         ;; prompt supplies the arguments; `apply' would pass none and
         ;; fail with "Wrong number of arguments".
         (if (and (null args) (commandp repl-fn))
             (call-interactively repl-fn)
           (apply repl-fn args))
         ;; 2. Identify the newly created REPL buffer
         (let* ((new-buffers (cl-set-difference (buffer-list) old-buffers))
                (repl-buf (car new-buffers))
                (proc (and repl-buf (get-buffer-process repl-buf))))
           (when proc
             ;; 3. Attach a sentinel to watch for process exit
             (set-process-sentinel
              proc
              (lambda (p event)
                (when (string-match-p "\\(?:status\\|exited\\|finished\\)" event)
                  (let ((buf (process-buffer p)))
                    (when (buffer-live-p buf)
                      ;; Kill the buffer, which automatically closes the side window
                      (kill-buffer buf))))))))))))

(provide 'popup-window)
;;; popup-window.el ends here
