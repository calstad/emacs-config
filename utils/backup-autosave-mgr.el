;; Keep tidy backup and autosave files.
;; Mostly taken from
;; http://amitp.blogspot.com/2007/03/emacs-move-autosave-and-backup-files.html

(defun set-backup-and-autosave-file-locale ()
  (let ((user-temporary-file-directory (concat dotfiles-dir ".backups-autosaves")))
    (make-directory user-temporary-file-directory t)
    (setq backup-by-copying t)
    (setq backup-directory-alist
          `((".*" . ,user-temporary-file-directory)
            (,tramp-file-name-regexp nil)))
    (setq auto-save-list-file-prefix
          (concat user-temporary-file-directory ".auto-saves-"))
    (setq auto-save-file-name-transforms
          `((".*" ,user-temporary-file-directory t)))))

(provide 'backup-autosave-mgr)

