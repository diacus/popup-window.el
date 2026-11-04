;;; popup-window-test.el --- Tests for popup-window.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   `emacs -Q --batch -L . -l test/popup-window-test.el -f ert-run-tests-batch-and-exit`

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'popup-window)

(defconst popup-window-test--repl-buffer-name "*popup-window-test-repl*"
  "Name of the buffer created by the fake REPLs.")

(defconst popup-window-test--aux-buffer-name "*popup-window-test-aux*"
  "Name of an auxiliary buffer a fake REPL may create.")

(defconst popup-window-test--sleep
  (or (executable-find "sleep") "/bin/sleep")
  "Path to the `sleep' executable used by the fake REPL processes.")

(defvar popup-window-test--repl-buf nil
  "Buffer the most recent fake REPL created.")

(defvar popup-window-test--processes nil
  "Processes started by fake REPLs, for cleanup.")

(defvar popup-window-test--seen-args nil
  "Arguments the most recent recording fake REPL received.")

(defun popup-window-test--make-repl (sleep-seconds &optional aux-mode)
  "Build a fake REPL function spawning `sleep SLEEP-SECONDS' in a buffer.
When AUX-MODE is `aux-first' or `aux-after', an auxiliary buffer
without a process is created before or after the REPL buffer,
so the macro sees an extra buffer alongside the REPL buffer.
Records the REPL
buffer in `popup-window-test--repl-buf'."
  (lambda (&rest _args)
    (when (eq aux-mode 'aux-first)
      (get-buffer-create popup-window-test--aux-buffer-name))
    (setq popup-window-test--repl-buf
          (get-buffer-create popup-window-test--repl-buffer-name))
    (let ((proc (start-process "popup-window-test-repl"
                               popup-window-test--repl-buf
                               popup-window-test--sleep
                               sleep-seconds)))
      (push proc popup-window-test--processes))
    (pop-to-buffer popup-window-test--repl-buf)
    (when (eq aux-mode 'aux-after)
      (get-buffer-create popup-window-test--aux-buffer-name))))

(defun popup-window-test--wait-for (predicate &optional timeout)
  "Wait up to TIMEOUT seconds for PREDICATE to return non-nil.
Run the command loop in between so process sentinels can fire.
Return PREDICATE's last value."
  (let ((deadline (+ (float-time) (or timeout 5.0)))
        (result nil))
    (while (and (not (setq result (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.05))
    result))

(defun popup-window-test--cleanup ()
  "Kill the buffers and processes created by the test suite."
  ;; Disable the "buffer has a running process; kill it?" query.
  (let ((process-kill-buffer-query-function #'ignore))
    ;; Read the variable so flycheck doesn't warn it is unused.
    (ignore process-kill-buffer-query-function)
    (dolist (buffer (buffer-list))
      (when (string-prefix-p "*popup-window-test" (buffer-name buffer))
        (when-let ((proc (get-buffer-process buffer)))
          (when (process-live-p proc)
            (delete-process proc)))
        (kill-buffer buffer))))
  (setq popup-window-test--processes nil
        popup-window-test--repl-buf nil))

;;; Macro expansion

(ert-deftest popup-window-test/expands-to-interactive-lambda ()
  "The expansion should be an interactive lambda taking (&rest args)."
  (let ((expansion (macroexpand-1 '(make-repl-popup 'foo-repl))))
    (should (eq (car expansion) 'lambda))
    (should (equal (nth 1 expansion) '(&rest args)))
    (should (member '(interactive) expansion))))

(ert-deftest popup-window-test/expansion-applies-args-to-repl-function ()
  "The expansion should apply ARGS to the REPL function."
  (let ((printed (prin1-to-string (macroexpand-1 '(make-repl-popup 'foo-repl)))))
    (should (string-match-p "(apply[^)]*foo-repl[^)]*args)" printed))))

(ert-deftest popup-window-test/forwards-args-to-repl-function ()
  "Arguments the popup is called with should reach the REPL function.
The expansion-level test above only inspects the generated code;
this one exercises the runtime `apply'."
  (let ((fn (make-repl-popup
             (lambda (&rest args) (setq popup-window-test--seen-args args)))))
    (setq popup-window-test--seen-args nil)
    (funcall fn "foo" 42)
    (should (equal popup-window-test--seen-args '("foo" 42)))))

;;; Buffer display

(ert-deftest popup-window-test/height-defaults-to-0-3 ()
  "`popup-window-height' should default to 0.3."
  (should (equal popup-window-height 0.3)))

(ert-deftest popup-window-test/height-option-is-honored ()
  "The displayed popup should honor a customized `popup-window-height'."
  (let ((popup-window-height 0.5)
        (fn (make-repl-popup (popup-window-test--make-repl "10"))))
    (funcall fn)
    (unwind-protect
        (let ((window (get-buffer-window popup-window-test--repl-buf)))
          (should (window-live-p window))
          (should (eq (window-parameter window 'window-side) 'bottom))
          ;; Allow a couple of lines of slack for rounding and
          ;; `window-min-height' clamping.
          (should (>= (window-total-height window)
                      (* 0.5 (frame-height) 0.9))))
      (popup-window-test--cleanup))))

(ert-deftest popup-window-test/pops-up-repls-that-switch-buffers ()
  "A REPL ending in `switch-to-buffer' should pop up too.
multi-vterm displays its fresh buffer that way, and plain
`switch-to-buffer' ignores the popup's `display-buffer-alist'
unless `switch-to-buffer-obey-display-actions' is on for the
call."
  :tags '(regression)
  (let ((fn (make-repl-popup
             (lambda (&rest _args)
               (setq popup-window-test--repl-buf
                     (get-buffer-create popup-window-test--repl-buffer-name))
               (let ((proc (start-process "popup-window-test-repl"
                                          popup-window-test--repl-buf
                                          popup-window-test--sleep "10")))
                 (push proc popup-window-test--processes))
               (switch-to-buffer popup-window-test--repl-buf)))))
    (funcall fn)
    (unwind-protect
        (let ((window (get-buffer-window popup-window-test--repl-buf)))
          (should (buffer-live-p popup-window-test--repl-buf))
          (should (window-live-p window))
          ;; The popup is the whole point: the buffer must not take
          ;; over the current window.
          (should (eq (window-parameter window 'window-side) 'bottom)))
      (popup-window-test--cleanup))))

(ert-deftest popup-window-test/displays-buffer-in-bottom-side-window ()
  "The REPL buffer should be displayed in a bottom side window."
  (let ((fn (make-repl-popup (popup-window-test--make-repl "10"))))
    (funcall fn)
    (unwind-protect
        (let ((window (get-buffer-window popup-window-test--repl-buf)))
          (should (buffer-live-p popup-window-test--repl-buf))
          (should (window-live-p window))
          (should (eq (window-parameter window 'window-side) 'bottom)))
      (popup-window-test--cleanup))))

(ert-deftest popup-window-test/displayed-aux-star-buffers-also-pop-up ()
  "Auxiliary *...*-buffers the REPL displays also pop up.
This is a regression pin of the current, documented limitation:
the `display-buffer-alist' the macro binds matches every
`*...*' buffer, not just the REPL's.  Narrowing the regex later
consciously breaks this test."
  :tags '(regression)
  (let ((fn (make-repl-popup
             (lambda (&rest _args)
               (setq popup-window-test--repl-buf
                     (get-buffer-create popup-window-test--repl-buffer-name))
               (let ((proc (start-process "popup-window-test-repl"
                                          popup-window-test--repl-buf
                                          popup-window-test--sleep "10")))
                 (push proc popup-window-test--processes))
               (pop-to-buffer popup-window-test--repl-buf)
               (display-buffer
                (get-buffer-create popup-window-test--aux-buffer-name))))))
    (funcall fn)
    (unwind-protect
        (let ((aux (get-buffer popup-window-test--aux-buffer-name)))
          (should (buffer-live-p popup-window-test--repl-buf))
          (should (buffer-live-p aux))
          ;; The aux buffer pops up in a bottom side window, like the
          ;; REPL buffer does.
          (let ((aux-window (get-buffer-window aux)))
            (should (window-live-p aux-window))
            (should (eq (window-parameter aux-window 'window-side)
                        'bottom))))
      (popup-window-test--cleanup))))

;;; Process cleanup

(ert-deftest popup-window-test/kills-buffer-and-window-on-process-exit ()
  "When the REPL process exits naturally, the buffer and window should go."
  (let ((fn (make-repl-popup (popup-window-test--make-repl "0.2"))))
    (funcall fn)
    (let ((buffer popup-window-test--repl-buf)
          (window (get-buffer-window popup-window-test--repl-buf)))
      (should (buffer-live-p buffer))
      (should (window-live-p window))
      ;; Wait for the process to exit and its sentinel to fire
      (should (popup-window-test--wait-for
               (lambda () (not (buffer-live-p buffer))) 5))
      (should (popup-window-test--wait-for
               (lambda () (not (window-live-p window))) 5)))))

(ert-deftest popup-window-test/aux-buffer-created-later-survives-repl-exit ()
  "The process sentinel should kill only the REPL buffer.
Buffers the REPL creates after the REPL buffer are not touched by
the cleanup."
  (let ((fn (make-repl-popup (popup-window-test--make-repl "0.2" 'aux-after))))
    (funcall fn)
    (unwind-protect
        (let ((buffer popup-window-test--repl-buf)
              (aux (get-buffer popup-window-test--aux-buffer-name)))
          (should (buffer-live-p aux))
          (should (popup-window-test--wait-for
                   (lambda () (not (buffer-live-p buffer))) 5))
          (should (buffer-live-p aux)))
      (popup-window-test--cleanup))))

(ert-deftest popup-window-test/aux-buffer-created-first-survives-repl-exit ()
  "An auxiliary buffer created before the REPL buffer is not cleaned up.
The macro locates the REPL buffer as the car of the buffer diff,
i.e., the most recently used among the new buffers.  Since the
fake REPL pops up its own buffer, it is still identified correctly
even when the aux buffer was created first."
  (let ((fn (make-repl-popup (popup-window-test--make-repl "0.2" 'aux-first))))
    (funcall fn)
    (unwind-protect
        (let ((buffer popup-window-test--repl-buf)
              (aux (get-buffer popup-window-test--aux-buffer-name)))
          (should (buffer-live-p buffer))
          (should (buffer-live-p aux))
          ;; Wait for the process to exit and its sentinel to fire
          (should (popup-window-test--wait-for
                   (lambda () (not (buffer-live-p buffer))) 5))
          (should (buffer-live-p aux)))
      (popup-window-test--cleanup))))

(ert-deftest popup-window-test/abnormal-process-exit-also-cleans-up ()
  "A REPL process exiting with a nonzero code should clean up too.
Its sentinel event (\"exited abnormally with code 2\") matches the
macro's `status|exited|finished' guard."
  (let ((fn (make-repl-popup
             (lambda (&rest _args)
               (setq popup-window-test--repl-buf
                     (get-buffer-create popup-window-test--repl-buffer-name))
               ;; /bin/sh -c "exit 2" terminates abnormally right away.
               (let ((proc (start-process "popup-window-test-repl"
                                          popup-window-test--repl-buf
                                          "/bin/sh" "-c" "exit 2")))
                 (push proc popup-window-test--processes))
               (pop-to-buffer popup-window-test--repl-buf)))))
    (funcall fn)
    (let ((buffer popup-window-test--repl-buf)
          (window (get-buffer-window popup-window-test--repl-buf)))
      (should (buffer-live-p buffer))
      (should (window-live-p window))
      (should (popup-window-test--wait-for
               (lambda () (not (buffer-live-p buffer))) 5))
      (should (popup-window-test--wait-for
               (lambda () (not (window-live-p window))) 5))
      (popup-window-test--cleanup))))

(ert-deftest popup-window-test/sentinel-ignores-non-exit-events ()
  "Process events not mentioning an exit state should not clean up.
The sentinel closes the popup only on `status', `exited' or
`finished' events; other status changes must leave the REPL alive."
  :tags '(regression)
  (let ((fn (make-repl-popup (popup-window-test--make-repl "10"))))
    (funcall fn)
    (unwind-protect
        (let* ((buffer popup-window-test--repl-buf)
               (window (get-buffer-window buffer))
               (proc (get-buffer-process buffer)))
          (should (process-live-p proc))
          ;; Feed the sentinel a benign status-change event directly.
          (funcall (process-sentinel proc) proc "foo bar\n")
          (should (buffer-live-p buffer))
          (should (window-live-p window)))
      (popup-window-test--cleanup))))

;;; Edge cases

(ert-deftest popup-window-test/repl-that-opens-no-buffer-does-not-error ()
  "A REPL function that creates no buffer should not error.
The buffer diff is empty, so the macro must cope with an empty
`cl-set-difference' result and skip the sentinel setup."
  (let ((fn (make-repl-popup (lambda (&rest _args) nil))))
    ;; `should' errors on nil, and the funcall returns whatever the
    ;; macro's cleanup branch yields, so wrap in a truthy progn.
    (should (progn (funcall fn "unused") t))))

(ert-deftest popup-window-test/buffer-without-process-survives ()
  "Buffers without a process should not be affected by the sentinel setup."
  (let ((fn (make-repl-popup
             (lambda (&rest _args)
               (setq popup-window-test--repl-buf
                     (get-buffer-create popup-window-test--repl-buffer-name))
               (pop-to-buffer popup-window-test--repl-buf)))))
    (funcall fn)
    (unwind-protect
        (progn
          (should (buffer-live-p popup-window-test--repl-buf))
          (should-not (get-buffer-process popup-window-test--repl-buf)))
      (popup-window-test--cleanup))))

;;; popup-window-test.el ends here