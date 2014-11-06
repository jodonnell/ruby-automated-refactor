(defvar ruby-automated-refactor--process-name "ruby-automated-refactor-process")

(defvar ruby-automated-refactor--method-name nil)
(defvar ruby-automated-refactor--method-current-buffer nil)
(defvar ruby-automated-refactor--method-call-marker nil)

(defvar ruby-automated-refactor--ruby-path
  (let ((current (buffer-file-name)))
    (expand-file-name (file-name-directory current)))
  "Path to the backend Ruby code.")

(defun ruby-automated-refactor-extract-method(method-name)
  (interactive "sNew method name: ")
  (setq ruby-automated-refactor--method-name method-name)
  (setq ruby-automated-refactor--method-current-buffer (current-buffer))
  (ruby-automated-refactor--setup-process)
  (ruby-automated-refactor--move-code-to-new-method method-name)
  (ruby-automated-refactor--add-arguments-to-method method-name))

(defun ruby-automated-refactor--add-arguments-to-method(method-name) ; bad method name
  (save-excursion
    (save-excursion
      (ruby-automated-refactor--old-method-into-ripper))
    (ruby-automated-refactor--new-method-into-ripper)
    (ruby-automated-refactor--get-used)))

(defun ruby-automated-refactor--setup-process()
  (when (not (and (ruby-automated-refactor--process) (process-live-p (ruby-automated-refactor--process))))
    (progn
      (ruby-automated-refactor--start-process)
      (ruby-automated-refactor--require-ruby-code))))

(defun ruby-automated-refactor--start-process()
  (let ((process-connection-type nil))  ; use a pipe
    (start-process ruby-automated-refactor--process-name nil "irb")
  (set-process-filter (ruby-automated-refactor--process) 'ruby-automated-refactor--process-filter)))
;(delete-process (ruby-automated-refactor--process))

(defun ruby-automated-refactor--require-ruby-code()
  (process-send-string (ruby-automated-refactor--process)
                       (format (mapconcat #'identity
                                          '("unless defined? ASTRefactor"
                                            "$:.unshift '%s'"
                                            "require 'ruby-automated-refactor'"
                                            "require 'ripper'"
                                            "ast_refactor = ASTRefactor.new"
                                            "end\n")
                                          "; ")
                               ruby-automated-refactor--ruby-path)))

(defun ruby-automated-refactor--process-filter(process output)
  (set-buffer ruby-automated-refactor--method-current-buffer)
  (if (string-match "\"\\(.*\\)\"$" output)
      (progn
      (let ((args (match-string 1 output)))
        (if (not (string= "" args))
            (progn
              (set-buffer ruby-automated-refactor--method-current-buffer)
              (beginning-of-buffer)
              (search-forward (concat "def " ruby-automated-refactor--method-name))
              (insert " ")
              (insert args)
              (goto-char ruby-automated-refactor--method-call-marker)
              (insert " ")
              (insert args)))))))

(defun ruby-automated-refactor--process()
  (get-process ruby-automated-refactor--process-name))

(defun ruby-automated-refactor--move-code-to-new-method(method-name)
  (save-excursion
    (ruby-automated-refactor--replace-region-with-method method-name)
    (ruby-automated-refactor--find-spot-to-insert-new-method)
    (ruby-automated-refactor--insert-new-method method-name)))

(defun ruby-automated-refactor--replace-region-with-method(method-name)
  (kill-region (point) (mark))
  (ruby-automated-refactor--insert-and-indent method-name)
  (setq ruby-automated-refactor--method-call-marker (point-marker))
  (newline))

(defun ruby-automated-refactor--find-spot-to-insert-new-method()
  (end-of-defun)
  (newline)
  (beginning-of-line))

(defun ruby-automated-refactor--insert-new-method(method-name)
  (ruby-automated-refactor--insert-and-indent (concat "def " method-name))
  (newline)
  (yank)
  (ruby-automated-refactor--insert-and-indent "end")
  (newline))

(defun ruby-automated-refactor--old-method-into-ripper()
  (beginning-of-defun)
  (process-send-string (ruby-automated-refactor--process) (ruby-automated-refactor--ripper-sexp "a")))

(defun ruby-automated-refactor--new-method-into-ripper()
  (end-of-defun)
  (process-send-string (ruby-automated-refactor--process) (ruby-automated-refactor--ripper-sexp "b")))

(defun ruby-automated-refactor--ripper-sexp(var)
  (concat var " = Ripper.sexp(<<ruby_emacs_extract_string\n" (ruby-automated-refactor--get-method) "\nruby_emacs_extract_string\n)\n"))

(defun ruby-automated-refactor--get-method() 
  (let (start)
    (setq start (point))
    (end-of-defun)
    (buffer-substring-no-properties start (point))))

(defun ruby-automated-refactor--get-used()
  (process-send-string (ruby-automated-refactor--process) "((ast_refactor.get_variable_references b, []) & (ast_refactor.get_local_variables_from_caller a, [])).join(', ')\n"))

(defun ruby-automated-refactor--insert-and-indent(text)
  (insert text)
  (indent-according-to-mode))
