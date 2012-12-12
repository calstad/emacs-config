;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'colin-remove-elc-on-save)
(add-hook 'emacs-lisp-mode-hook 'custom-prog-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)

(defun colin-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

;; Customizations for interactive elisp mode
(defun colin-ielm-mode-hook ()
  (colin-turn-on-paredit)
  (turn-on-eldoc-mode))

(add-hook 'ielm-mode-hook 'colin-ielm-mode-hook)

(define-key emacs-lisp-mode-map (kbd "C-c v") 'eval-buffer)

;; General Lisp configs
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

;; Turn on paredit for all lisp modes.
(dolist (mode '(scheme emacs-lisp lisp clojure clojurescript))
  (when (> (display-color-cells) 8)
    (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
              'colin-turn-on-paredit)))

;;SLIME repl
;; Fix paredit keybindings
(defun override-slime-repl-bindings-with-paredit ()
  (progn
    (define-key slime-repl-mode-map
      (read-kbd-macro paredit-backward-delete-key) nil)
    (define-key slime-repl-mode-map
      (kbd "M-s") 'paredit-splice-sexp)))

;; Add clojure paredit support for vectors and maps
(defun fix-paredit-repl ()
  (interactive)
  (local-set-key "{" 'paredit-open-curly)
  (local-set-key "}" 'paredit-close-curly)
  (modify-syntax-entry ?\{ "(}") 
  (modify-syntax-entry ?\} "){")
  (modify-syntax-entry ?\[ "(]")
  (modify-syntax-entry ?\] ")["))

(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)
(add-hook 'slime-repl-mode-hook 'fix-paredit-repl)
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

;; Clojure nREPL
(add-hook 'nrepl-interaction-mode-hook
          'nrepl-turn-on-eldoc-mode)
(setq nrepl-popup-stacktraces nil)
 (add-to-list 'same-window-buffer-names "*nrepl*")

(provide 'lisp-config)
