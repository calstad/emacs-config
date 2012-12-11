;; Hooks for all programming modes
(add-hook 'prog-mode-hook 'cf-local-column-number-mode)
(add-hook 'prog-mode-hook 'cf-local-comment-auto-fill)
(add-hook 'prog-mode-hook 'cf-turn-on-hl-line-mode)
(add-hook 'prog-mode-hook 'cf-pretty-lambdas)
(add-hook 'prog-mode-hook 'cf-add-watchwords)

(defun custom-prog-mode-hook ()
  (run-hooks 'prog-mode-hook))

;; Autoload octave mode for octave files.
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

(require 'magit-config)
(require 'lisp-config)
(require 'ruby-config)

(provide 'programming-config)
