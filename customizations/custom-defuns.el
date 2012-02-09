;; Using cf- (custom function) prefix for defun names to prevent name collision.
;; All initial stolen from starter kit and prelude.

(defun cf-local-column-number-mode ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t))

(defun cf-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun cf-turn-on-hl-line-mode ()
  (when (> (display-color-cells) 8)
    (hl-line-mode t)))

(defun cf-turn-on-whitespace ()
  (whitespace-mode t))

(defun cf-turn-on-paredit ()
  (paredit-mode t))

(defun cf-pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun cf-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(defun cf-sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; Buffer cleanup functions
(defun cf-untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun cf-indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cf-cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (cf-indent-buffer)
  (cf-untabify-buffer)
  (delete-trailing-whitespace))

(provide 'custom-defuns)


