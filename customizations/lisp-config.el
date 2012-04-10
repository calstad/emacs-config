(ensure-package 'paredit)
(ensure-package 'clojure-mode)
(ensure-package 'clojure-test-mode)
(ensure-package 'clojurescript-mode)
(ensure-package 'elisp-slime-nav)
(ensure-package 'slime-repl)

;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'cf-remove-elc-on-save)
(add-hook 'emacs-lisp-mode-hook 'custom-prog-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)

(defun cf-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

;; Customizations for interactive elisp mode
(defun cf-ielm-mode-hook ()
  (cf-turn-on-paredit)
  (turn-on-eldoc-mode))

(add-hook 'ielm-mode-hook 'cf-ielm-mode-hook)

(define-key emacs-lisp-mode-map (kbd "C-c v") 'eval-buffer)

;; General Lisp configs

(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

;; Turn on paredit for all lisp modes.
(dolist (mode '(scheme emacs-lisp lisp clojure clojurescript))
  (when (> (display-color-cells) 8)
    (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
              'cf-turn-on-paredit)))

;; Use paredit in SLIME repl
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

(provide 'lisp-config)
