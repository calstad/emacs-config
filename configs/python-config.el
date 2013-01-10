;; Use python.el that will be default in 24.3
(vendor 'python)

;; Setup Jedi for dev support
(setq jedi:setup-keys t)
(vendor 'jedi)
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)

;; Turn off electric indention due to conflict with python-mode
(add-hook 'python-mode-hook
          #'(lambda () (setq electric-indent-mode nil)))

(provide 'python-config)


