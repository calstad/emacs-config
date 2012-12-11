(colin-add-package 'yaml-mode)
(colin-add-package 'inf-ruby)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(vendor 'rinari)

(provide 'ruby-config)
