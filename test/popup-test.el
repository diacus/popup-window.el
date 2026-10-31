;;; popup-test.el --- Tests for popup.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   `emacs -Q --batch -L . -l test/popup-test.el -f ert-run-tests-batch-and-exit`

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'popup)

(defconst popup-test--repl-buffer-name "*popup-test-repl*"
  "Name of the buffer created by the fake REPLs.")

(defconst popup-test--aux-buffer-name "*popup-test-aux*"
  "Name of an auxiliary buffer a fake REPL may create.")

(defconst popup-test--sleep
  (or (executable-find "sleep") "/bin/sleep")
  "Path to the `sleep' executable used by the fake REPL processes.")

(defvar popup-test--repl-buf nil
  "Buffer the most recent fake REPL created.")

(defvar popup-test--processes nil
  "Processes started by fake REPLs, for cleanup.")

(defun popup-test--make-repl (sleep-seconds &optional aux-mode)
  "Build a fake REPL function spawning `sleep SLEEP-SECONDS' in a buffer.
When AUX-MODE is `aux-first' or `aux-after', an auxiliary buffer
without a process is created before or after the REPL buffer,
so the macro sees an extra buffer alongside the REPL buffer.
Records the REPL
buffer in `popup-test--repl-buf'."
  (lambda (&rest _args)
    (when (eq aux-mode 'aux-first)
      (get-buffer-create popup-test--aux-buffer-name))
    (setq popup-test--repl-buf
          (get-buffer-create popup-test--repl-buffer-name))
    (let ((proc (start-process "popup-test-repl"
                               popup-test--repl-buf
                               popup-test--sleep
                               sleep-seconds)))
      (push proc popup-test--processes))
    (pop-to-buffer popup-test--repl-buf)
    (when (eq aux-mode 'aux-after)
      (get-buffer-create popup-test--aux-buffer-name))))

(defun popup-test--wait-for (predicate &optional timeout)
  "Wait up to TIMEOUT seconds for PREDICATE to return non-nil.
Run the command loop in between so process sentinels can fire.
Return PREDICATE's last value."
  (let ((deadline (+ (float-time) (or timeout 5.0)))
        (result nil))
    (while (and (not (setq result (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.05))
    result))

(defun popup-test--cleanup ()
  "Kill the buffers and processes created by the test suite."
  ;; Disable the "buffer has a running process; kill it?" query.
  (let ((process-kill-buffer-query-function #'ignore))
    ;; Read the variable so flycheck doesn't warn it is unused.
    (ignore process-kill-buffer-query-function)
    (dolist (buffer (buffer-list))
      (when (string-prefix-p "*popup-test" (buffer-name buffer))
        (when-let ((proc (get-buffer-process buffer)))
          (when (process-live-p proc)
            (delete-process proc)))
        (kill-buffer buffer))))
  (setq popup-test--processes nil
        popup-test--repl-buf nil))

;;; Macro expansion

(ert-deftest popup-test/expands-to-interactive-lambda ()
  "The expansion should be an interactive lambda taking (&rest args)."
  (let ((expansion (macroexpand-1 '(make-repl-popup 'foo-repl))))
    (should (eq (car expansion) 'lambda))
    (should (equal (nth 1 expansion) '(&rest args)))
    (should (member '(interactive) expansion))))

(ert-deftest popup-test/expansion-applies-args-to-repl-function ()
  "The expansion should apply ARGS to the REPL function."
  (let ((printed (prin1-to-string (macroexpand-1 '(make-repl-popup 'foo-repl)))))
    (should (string-match-p "(apply[^)]*foo-repl[^)]*args)" printed))))

;;; Buffer display

(ert-deftest popup-test/displays-buffer-in-bottom-side-window ()
  "The REPL buffer should be displayed in a bottom side window."
  (let ((fn (make-repl-popup (popup-test--make-repl "10"))))
    (funcall fn)
    (unwind-protect
        (let ((window (get-buffer-window popup-test--repl-buf)))
          (should (buffer-live-p popup-test--repl-buf))
          (should (window-live-p window))
          (should (eq (window-parameter window 'window-side) 'bottom)))
      (popup-test--cleanup))))

;;; Process cleanup

(ert-deftest popup-test/kills-buffer-and-window-on-process-exit ()
  "When the REPL process exits naturally, the buffer and window should go."
  (let ((fn (make-repl-popup (popup-test--make-repl "0.2"))))
    (funcall fn)
    (let ((buffer popup-test--repl-buf)
          (window (get-buffer-window popup-test--repl-buf)))
      (should (buffer-live-p buffer))
      (should (window-live-p window))
      ;; Wait for the process to exit and its sentinel to fire
      (should (popup-test--wait-for
               (lambda () (not (buffer-live-p buffer))) 5))
      (should (popup-test--wait-for
               (lambda () (not (window-live-p window))) 5)))))

(ert-deftest popup-test/aux-buffer-created-later-survives-repl-exit ()
  "The process sentinel should kill only the REPL buffer.
Buffers the REPL creates after the REPL buffer are not touched by
the cleanup."
  (let ((fn (make-repl-popup (popup-test--make-repl "0.2" 'aux-after))))
    (funcall fn)
    (unwind-protect
        (let ((buffer popup-test--repl-buf)
              (aux (get-buffer popup-test--aux-buffer-name)))
          (should (buffer-live-p aux))
          (should (popup-test--wait-for
                   (lambda () (not (buffer-live-p buffer))) 5))
          (should (buffer-live-p aux)))
      (popup-test--cleanup))))

(ert-deftest popup-test/aux-buffer-created-first-survives-repl-exit ()
  "An auxiliary buffer created before the REPL buffer is not cleaned up.
The macro locates the REPL buffer as the car of the buffer diff,
i.e., the most recently used among the new buffers.  Since the
fake REPL pops up its own buffer, it is still identified correctly
even when the aux buffer was created first."
  (let ((fn (make-repl-popup (popup-test--make-repl "0.2" 'aux-first))))
    (funcall fn)
    (unwind-protect
        (let ((buffer popup-test--repl-buf)
              (aux (get-buffer popup-test--aux-buffer-name)))
          (should (buffer-live-p buffer))
          (should (buffer-live-p aux))
          ;; Wait for the process to exit and its sentinel to fire
          (should (popup-test--wait-for
                   (lambda () (not (buffer-live-p buffer))) 5))
          (should (buffer-live-p aux)))
      (popup-test--cleanup))))

(ert-deftest popup-test/buffer-without-process-survives ()
  "Buffers without a process should not be affected by the sentinel setup."
  (let ((fn (make-repl-popup
             (lambda (&rest _args)
               (setq popup-test--repl-buf
                     (get-buffer-create popup-test--repl-buffer-name))
               (pop-to-buffer popup-test--repl-buf)))))
    (funcall fn)
    (unwind-protect
        (progn
          (should (buffer-live-p popup-test--repl-buf))
          (should-not (get-buffer-process popup-test--repl-buf)))
      (popup-test--cleanup))))

;;; popup-test.el ends here