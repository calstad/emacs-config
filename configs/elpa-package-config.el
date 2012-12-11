;; Load up ELPA, the package manager with marmalade

(setq package-user-dir (concat colin-dotfiles-dir "elpa"))
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Packages to be installed
(defvar colin-packages '())

(defun colin-add-package (package-name)
  (add-to-list 'colin-packages package-name))

(defun colin-package-not-installed-p (pkg)
  (not (package-installed-p pkg)))

(defun colin-install-needed-packages ()
  (dolist (pkg (remove-if-not 'colin-package-not-installed-p colin-packages))
    (package-install pkg)))

(provide 'elpa-package-config)
