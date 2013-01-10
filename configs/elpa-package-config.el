;; Load up ELPA, the package manager with marmalade

(setq package-user-dir (concat calstad-dotfiles-dir "elpa"))
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Packages to be installed
(defconst calstad-elpa-dependencies
  '(;; General Editor stuff
    find-file-in-project
    ido-ubiquitous
    smex
    magit
    yasnippet

    ;; Lisp
    paredit
    clojure-mode
    clojure-test-mode
    clojurescript-mode
    elisp-slime-nav
    slime-repl
    nrepl

    ;; Ruby
    yaml-mode
    inf-ruby
    haml-mode
    rinari

    ;; Python
    virtualenv
    ))

(defun calstad-needed-packages ()
  (remove-if 'package-installed-p calstad-elpa-dependencies))

(defun calstad-install-needed-packages ()
  (let ((needed-pkgs (calstad-needed-packages)))
    (when needed-pkgs
      (package-refresh-contents)
      (dolist (pkg needed-pkgs)
        (package-install pkg)))))

(provide 'elpa-package-config)
