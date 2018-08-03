;; [{shell,apply_fun,3,[{file,"shell.erl"},{line,907}]},
;;  {erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,681}]},
;;  {erl_eval,try_clauses,8,[{file,"erl_eval.erl"},{line,911}]},
;;  {shell,exprs,7,[{file,"shell.erl"},{line,686}]},{shell,eval_exprs,7,[{file,"shell.erl"},{line,642}]},
;;  {shell,eval_loop,3,[{file,"shell.erl"},{line,627}]}]

(require 'dash)

(defun erlstack-whitespacify-concat (&rest re)
  "Add whitespace match between to Erlang term regexp"
  (--reduce (concat acc "[[:space:]]*" it) re))

(defvar erlstack-overlay nil)

(defvar erlstack-code-overlay nil)

(defvar erlstack-code-buffer "*Erlstack code*")

(defvar erlstack-escaped-string-re
  "\"\\(\\([^\"]\\|\\\"\\)*\\)\"")

(defvar erlstack-file-re
  (concat "{file," erlstack-escaped-string-re "}"))

(defvar erlstack-line-re
  "{line,\\([[:digit:]]+\\)}")

(defvar erlstack-position-re
  (erlstack-whitespacify-concat "\\[" erlstack-file-re "," erlstack-line-re "]"))

(defvar erlstack-stack-frame-re
  (erlstack-whitespacify-concat "{.*" erlstack-position-re "}"))

(defvar erlstack-stack-end-re
  "}]}")

(defcustom erlstack-file-search-hook
  '(erlstack-try-fullname)
  "List of hooks used to search project files"
  :options '(erlstack-try-fullname erlstack-remote-query)
  :group 'erlstack
  :type 'hook)

(defcustom erlstack-lookup-window
  300
  "Size of the lookup window"
  :group 'erlstack
  :type 'integer)

(defface erlstack-frame-face
  '((((background light))
     :background "orange"
     :foreground "darkred")
    (((background dark))
     :background "orange"
     :foreground "red"))
  "The face for matched `erlstack' stack frame")

(defun erlstack-frame-found (begin end)
  "This fuction is called when point enters stack frame"
  (let ((query       (match-string 1))
        (line-number (string-to-number (match-string 3))))
    (erlstack-try-show-file query line-number)
    (setq erlstack-overlay (make-overlay begin end))
    (overlay-put erlstack-overlay 'face 'erlstack-frame-face)))

(defun erlstack-try-show-file (query line-number)
  "Search for a file"
  (let ((filename
         (run-hook-with-args-until-success 'erlstack-file-search-hook query line-number)))
    (when filename
      (with-current-buffer (get-buffer-create erlstack-code-buffer)
        (erase-buffer)
        (insert-file-contents filename)
        (with-no-warnings
          (goto-line line-number))
        (setq erlstack-code-buffer-posn (point))
        (when erlstack-code-overlay
          (delete-overlay erlstack-code-overlay))
        (setq erlstack-code-overlay (make-overlay
                                     (line-beginning-position)
                                     (line-end-position)))
        (overlay-put erlstack-code-overlay 'face 'erlstack-frame-face)
        (erlang-mode))
      (set-window-point (display-buffer erlstack-code-buffer)
                        erlstack-code-buffer-posn))))

(defun erlstack-try-fullname (query line)
  "Try search for local file matching full path"
  (when (file-exists-p query)
    query))

(defun erlstack-frame-lost ()
  "This fuction is called when point leaves stack frame")

(defun erlstack-run-at-point ()
  "Attempt to analyze stack frame at the point"
  (interactive)
  (when erlstack-overlay
    (delete-overlay erlstack-overlay))
  (while-no-input
    (pcase (erlstack-parse-at-point)
      (`(,begin ,end) (erlstack-frame-found begin end))
      (_              (erlstack-frame-lost)))))

(defun erlstack-parse-at-point ()
  "Attempt to find stacktrace at point"
  (save-excursion
    (let ((begin (re-search-forward erlstack-stack-end-re
                                    (+ (point) erlstack-lookup-window) t))
          (end   (re-search-backward erlstack-stack-frame-re
                                     (- (point) erlstack-lookup-window) t)))
      (when (and begin end)
        `(,begin ,end)))))

(define-minor-mode erlstack-mode
 "Parse Erlang stacktrace under point and quickly navigate to the
line of the code"
 :keymap nil
 :group 'erlstack
 :lighter " es"
 :global t
 (add-hook 'post-command-hook #'erlstack-run-at-point))
