;; Uitil functions to make sure packages are installed from elpa

(defun ensure-package (package-name)
  (when (not (package-installed-p package-name))
    (package-install package-name)))

(provide 'elpa-utils)
