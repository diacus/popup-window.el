;;; popup-window.el --- Runs helper tool within popup window -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Diacus Magnuz

;; Author: Diacus Magnuz <diacus.magnuz@gmail.com>
;; Maintainer: Diacus Magnuz <diacus.magnuz@gmail.com>
;; URL: https://github.com/diacus/popup.el
;; Version: 0.2.0
;; Package-Version: 20261101.45
;; Package-Requires: ((emacs "24.3"))
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

(defmacro make-repl-popup (repl-function)
  "Run REPL-FUNCTION that inside a pop-up window."
  `(lambda (&rest args)
     (interactive)
     (let ((display-buffer-alist
            '(("\\*.*\\*"
               (display-buffer-in-side-window)
               (side . bottom)
               (window-height . 0.3))))
           ;; Some REPLs switch with `switch-to-buffer' (multi-vterm
           ;; does), which never consults `display-buffer-alist'
           ;; unless this option is on: without it the popup never
           ;; happens and the buffer takes over the current window.
           (switch-to-buffer-obey-display-actions t)
           (old-buffers (buffer-list)))
       ;; 1. Run the REPL function
       (apply ,repl-function args)
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
                    (kill-buffer buf)))))))))))

(provide 'popup-window)
;;; popup-window.el ends here
