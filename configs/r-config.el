;; Setup emacs speacks statistics
(load (concat calstad-vendor-dir "/ess/lisp/ess-site"))
(require 'ess-site)

(setq ess-history-directory (concat calstad-temporary-file-directory "ess"))

(provide 'r-config)
