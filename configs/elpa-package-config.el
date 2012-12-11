;; Load up ELPA, the package manager with marmalade

(setq package-user-dir (concat colin-dotfiles-dir "elpa"))
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Packages to be installed
(defvar colin-packages '())

(defun colin-add-package (package-name)
  (add-to-list 'colin-packages))

(defun colin-package-not-installed-p (p)
  (not (package-installed-p p)))

(defun colin-check-packages ()
  ())

(unless (tad-packages-installed-p)
  ;; check for new packages (package version)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p colin-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'elpa-package-config.el)
