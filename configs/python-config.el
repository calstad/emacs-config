;; Use python.el that will be default in 24.3
(vendor 'python)

;; Setup Jedi for dev support
(setq jedi:setup-keys t)
(vendor 'jedi)
(autoload 'jedi:setup "jedi" nil t)

(defun calstad-python-mode-defaults ()
  (jedi:setup)
  (electric-indent-mode -1))

(setq calstad-python-mode-hook 'calstad-python-mode-defaults)

(add-hook 'python-mode-hook (lambda ()
                              (run-hooks 'calstad-python-mode-hook)))

(provide 'python-config)


