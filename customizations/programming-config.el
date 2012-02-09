;; Hooks for all programming modes
(add-hook 'prog-mode-hook 'cf-local-column-number-mode)
(add-hook 'prog-mode-hook 'cf-local-comment-auto-fill)
(add-hook 'prog-mode-hook 'cf-turn-on-hl-line-mode)
(add-hook 'prog-mode-hook 'cf-pretty-lambdas)
(add-hook 'prog-mode-hook 'cf-add-watchwords)

(defun custom-prog-mode-hook ()
  (run-hooks 'prog-mode-hook))

(require 'magit-config)
(require 'lisp-config)

(provide 'programming-config)
