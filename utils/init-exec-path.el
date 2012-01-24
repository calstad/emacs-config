;; Taken from http://stackoverflow.com/a/6415812
;; Function to ensure that all places on the bash PATH are added to emacs exec-path variable.

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(provide 'init-exec-path)
