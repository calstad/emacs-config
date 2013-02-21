;; Coffeescript config

(defun calstad-coffee-mode-defaults ()
  (electric-indent-mode -1)
  (setq tab-width 2))

(setq calstad-coffee-mode-hook 'calstad-coffee-mode-defaults)
(add-hook 'coffee-mode-hook (lambda ()
                              (run-hooks 'calstad-coffee-mode-hook)))

(provide 'javascript-config)
