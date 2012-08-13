(ensure-package 'yaml-mode)
(ensure-package 'inf-ruby)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(provide 'ruby-config)
