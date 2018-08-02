;; [{shell,apply_fun,3,[{file,"shell.erl"},{line,907}]},
;;  {erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,681}]},
;;  {erl_eval,try_clauses,8,[{file,"erl_eval.erl"},{line,911}]},
;;  {shell,exprs,7,[{file,"shell.erl"},{line,686}]},
;;  {shell,eval_exprs,7,[{file,"shell.erl"},{line,642}]},
;;  {shell,eval_loop,3,[{file,"shell.erl"},{line,627}]}]

(defvar erlstack-escaped-string-re
  "\"\\(\\([^\"]\\|\\\"\\)*\\)\"")

(defvar erlstack-file-re
  (concat "{file," erlstack-escaped-string-re "}"))

(defvar erlstack-line-re
  "{line,\\([[:digit:]]+\\)}")

(defvar erlstack-position-re
  (concat "\\[" erlstack-file-re "," erlstack-line-re "]"))

(defvar erlstack-stack-end-re
  "}]}")

(defcustom erlstack-search-limit
  60
  "Maximum length of the stacktrace entry")

(defface erlstack-frame-face
  '((((background light))
     :background "orange"
     :foreground "darkred")
    (((background dark))
     :background "orange"
     :foreground "red"))
  "The face for matched `erlstack' stack frame")

(defun erlstack-run-at-point ()
  "Attempt to analyze stack at point"
  (interactive)
  (while-no-input
    (when erlstack-overlay
      (delete-overlay erlstack-overlay))
    (pcase (erlstack-parse-at-point)
      (`(,begin ,end)
       (progn
         (erlstack-try-open-file (erlstack-extract-matched))
         (setq erlstack-overlay (make-overlay begin end))
         (overlay-put erlstack-overlay 'face 'erlstack-frame-face)))
      (_ t))))

(defun erlstack-extract-matched ()
  `(,(match-string 1)
    ,(string-to-number (match-string 3))))

(defun erlstack-parse-at-point ()
  "Attempt to find stacktrace at point"
  (save-excursion
    (let ((begin (re-search-forward erlstack-stack-end-re (line-end-position) t))
          (end   (re-search-backward erlstack-position-re (line-beginning-position) t)))
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
