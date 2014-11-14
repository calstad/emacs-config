;; Using calstad- prefix for defun names to prevent name collision.
;; Most stolen from starter kit and prelude.

(defun calstad-local-column-number-mode ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t))

(defun calstad-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun calstad-turn-on-hl-line-mode ()
  (when (> (display-color-cells) 8)
    (hl-line-mode t)))

(defun calstad-turn-on-whitespace ()
  (whitespace-mode t))

(defun calstad-turn-on-paredit ()
  (paredit-mode t))

(defun calstad-turn-off-electric-indent ()
  (set (make-local-variable 'electric-indent-mode) nil))

(defun calstad-pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun calstad-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\|FIX\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(defun calstad-sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; Buffer cleanup functions
(defun calstad-untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun calstad-indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun calstad-cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (calstad-indent-buffer)
  (calstad-untabify-buffer)
  (delete-trailing-whitespace))

(defun calstad-swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (if (/= (count-windows) 2)
      (message "You need exactly 2 windows to do this.")
    (let* ((w1 (first (window-list)))
           (w2 (second (window-list)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)))
  (other-window 1))

(defun calstad-kill-other-buffers ()
  "Kill all buffers but the current one. Doesn't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

(defun calstad-delete-file-and-buffer ()
  "Kills the current buffer and deletes the file it is visiting"
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (delete-file filename)
      (message "Deleted file %s" filename)))
  (kill-buffer))

(provide 'custom-defuns)


