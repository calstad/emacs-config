;; (when window-system
;;   (setq frame-title-format '(buffer-file-name "%f" ("%b")))
;;   (tooltip-mode -1)
;;   (mouse-wheel-mode t)
;;   (blink-cursor-mode -1))
;;

(setq visible-bell t
      inhibit-startup-message t)

;; Set zenburn as the color theme
(setq color-theme-is-global t)
(ensure-package 'zenburn-theme)
(load-theme 'zenburn t)

(provide 'my-ui)
