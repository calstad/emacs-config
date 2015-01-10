;; Hooks for all programming modes
(add-hook 'prog-mode-hook 'calstad-local-column-number-mode)
(add-hook 'prog-mode-hook 'calstad-local-comment-auto-fill)
(add-hook 'prog-mode-hook 'calstad-turn-on-hl-line-mode)
(add-hook 'prog-mode-hook 'calstad-pretty-lambdas)
(add-hook 'prog-mode-hook 'calstad-add-watchwords)
(add-hook 'prog-mode-hook (lambda () (yas-minor-mode)))

(defun custom-prog-mode-hook ()
  (run-hooks 'prog-mode-hook))

;; Autoload octave mode for octave files.
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

(require 'magit-config)
(require 'lisp-config)
(require 'ruby-config)
(require 'python-config)
(require 'javascript-config)

(provide 'programming-config)
