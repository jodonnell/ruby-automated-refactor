(defvar extract-process-name "extract-process")

(defvar extract-method-name nil)
(defvar extract-method-current-buffer nil)
(defvar extract-method-call-marker nil)

(defvar extract-ruby-path
  (let ((current (buffer-file-name)))
    (expand-file-name (file-name-directory current)))
  "Path to the backend Ruby code.")

(defun extract-method(method-name)
  (interactive "sNew method name: ")
  (setq extract-method-name method-name)
  (setq extract-method-current-buffer (current-buffer))
  (setup-process)
  (move-code-to-new-method method-name)
  (add-arguments-to-method method-name))

(defun add-arguments-to-method(method-name) ; bad method name
  (save-excursion
    (save-excursion
      (old-method-into-ripper))
    (new-method-into-ripper)
    (get-used)))

(defun setup-process()
  (when (not (and (extract-process) (process-live-p (extract-process))))
    (progn
      (start-extract-process)
      (require-ruby-code))))

(defun start-extract-process()
  (let ((process-connection-type nil))  ; use a pipe
    (start-process extract-process-name nil "irb")
  (set-process-filter (extract-process) 'extract-process-filter)))
;(delete-process (extract-process))

(defun require-ruby-code()
  (process-send-string (extract-process)
                       (format (mapconcat #'identity
                                          '("unless defined? ASTRefactor"
                                            "$:.unshift '%s'"
                                            "require 'extract'"
                                            "require 'ripper'"
                                            "ast_refactor = ASTRefactor.new"
                                            "end\n")
                                          "; ")
                               extract-ruby-path)))

(defun extract-process-filter(process output)
  (set-buffer extract-method-current-buffer)
  (if (string-match "\"\\(.*\\)\"$" output)
      (progn
      (let ((args (match-string 1 output)))
        (set-buffer extract-method-current-buffer)
        (beginning-of-buffer)
        (search-forward (concat "def " extract-method-name))
        (insert " ")
        (insert args)
        (goto-char extract-method-call-marker)
        (insert " ")
        (insert args)))))

(defun extract-process()
  (get-process extract-process-name))

(defun move-code-to-new-method(method-name)
  (save-excursion
    (replace-region-with-method method-name)
    (find-spot-to-insert-new-method)
    (insert-new-method method-name)))

(defun replace-region-with-method(method-name)
  (kill-region (point) (mark))
  (insert-and-indent method-name)
  (setq extract-method-call-marker (point-marker))
  (newline))

(defun find-spot-to-insert-new-method()
  (end-of-defun)
  (newline)
  (beginning-of-line))

(defun insert-new-method(method-name)
  (insert-and-indent (concat "def " method-name))
  (newline)
  (yank)
  (insert-and-indent "end")
  (newline))

(defun old-method-into-ripper()
  (beginning-of-defun)
  (process-send-string (extract-process) (ripper-sexp "a")))

(defun new-method-into-ripper()
  (end-of-defun)
  (process-send-string (extract-process) (ripper-sexp "b")))

(defun ripper-sexp(var)
  (concat var " = Ripper.sexp(<<ruby_emacs_extract_string\n" (get-method) "\nruby_emacs_extract_string\n)\n"))

(defun get-method() 
  ; need to escape any '
  (let (start)
    (setq start (point))
    (end-of-defun)
    (buffer-substring-no-properties start (point))))

(defun get-used()
  (process-send-string (extract-process) "((ast_refactor.get_variable_references b, []) & (ast_refactor.get_local_variables_from_caller a, [])).join(', ')\n"))

(defun insert-and-indent(text)
  (insert text)
  (indent-according-to-mode))
