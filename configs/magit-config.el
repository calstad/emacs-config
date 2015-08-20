;; Customizations for the magit emacs interface to git.

;; Set keybinding for magit status
(global-set-key (kbd "C-x g") 'magit-status)

;; Keep file revert warning from showing everytime magit starts.
(setq magit-last-seen-setup-instructions "1.4.0")

;; Diff colors
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))


(provide 'magit-config)
