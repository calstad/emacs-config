(defun calstad-python-mode-defaults ()
  (calstad-turn-off-electric-indent))

(setq calstad-python-mode-hook 'calstad-python-mode-defaults)

(add-hook 'python-mode-hook (lambda ()
                              (run-hooks 'calstad-python-mode-hook)))

(provide 'python-config)


