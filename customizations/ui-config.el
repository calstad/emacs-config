(vendor 'maxframe)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)       ; dont need mouse tootips!
  (mouse-wheel-mode t)    ; stupid mice
  (blink-cursor-mode -1)  ; about to give me seizures
  (add-hook 'window-setup-hook 'maximize-frame t))

;; Dont play audio bell
(setq visible-bell t)

;; Disable startup screen
(setq inhibit-startup-message t)

;; Mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Add indication at bottom of buffer for empty lines
(set-default 'indicate-empty-lines t)

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Custom Emacs 24 color themes support
(add-to-list 'custom-theme-load-path (concat dotfiles-dir "themes/"))
(setq color-theme-is-global t)

;; Set zenburn as the color theme
(load-theme 'zenburn t)

;; Better status line
(vendor 'powerline)

(provide 'ui-config)
