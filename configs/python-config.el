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

(defun calstad-python-mode-defaults ()
  (calstad-turn-off-electric-indent))

(setq calstad-python-mode-hook 'calstad-python-mode-defaults)

(add-hook 'python-mode-hook (lambda ()
                              (run-hooks 'calstad-python-mode-hook)))

(provide 'python-config)


