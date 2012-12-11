;; Customizations for the magit emacs interface to git.

;; Make sure the package is installed
(colin-add-package 'magit)

;; Set keybinding for magit status
(global-set-key (kbd "C-x g") 'magit-status)

;; Diff colors
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))

(provide 'magit-config)
