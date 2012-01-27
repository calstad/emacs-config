(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

;; Dont play audio bell
(setq visible-bell t)

;; Disable startup screen
(setq inhibit-startup-message t)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Custom Emacs 24 color themes support
(add-to-list 'custom-theme-load-path (concat dotfiles-dir "themes/"))
(setq color-theme-is-global t)

;; Set zenburn as the color theme
;;(load-theme 'zenburn t)

(provide 'my-ui)
