;; Set IPython as inferior-python process.
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;; Use default indent level of 4 spaces
(setq python-indent-guess-indent-offset nil)

;; Emacs IPython Notebook settings
(setq
 ein:notebook-modes '(ein:notebook-mumamo-mode ein:notebook-python-mode)
 ein:use-auto-complete t
 ein:console-security-dir "/Users/colin/.ipython/profile_default/security"
 ein:console-args '("--profile" "default")
)

;; Emacs IPython Notebook keybindings
(add-hook 'ein:notebook-python-mode-hook
  #'(lambda ()
      (define-key python-mode-map "\C-m" 'newline-and-indent)))

(defun calstad-python-mode-defaults ()
  (calstad-turn-off-electric-indent))

(setq calstad-python-mode-hook 'calstad-python-mode-defaults)

(add-hook 'python-mode-hook (lambda ()
                              (run-hooks 'calstad-python-mode-hook)))

(provide 'python-config)


