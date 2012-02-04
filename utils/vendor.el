;; Mostly taken from https://github.com/al3x/emacs.git
;; For loading libraries in from the vendor directory

(defun vendor (library)
  (let* ((file (symbol-name library))
         (normal (concat vendor-dir "/" file))
         (suffix (concat normal ".el")))
    (cond
     ((file-directory-p normal)
      (add-to-list 'load-path normal)
      (require library))
     ((file-directory-p suffix)
      (add-to-list 'load-path suffix)
      (require library))
     ((file-exists-p suffix)
      (require library)))))

(provide 'vendor)
