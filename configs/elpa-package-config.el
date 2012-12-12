;; Load up ELPA, the package manager with marmalade

(setq package-user-dir (concat colin-dotfiles-dir "elpa"))
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Packages to be installed
(defconst colin-elpa-dependencies
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
    ))

(defun colin-needed-packages ()
  (remove-if 'package-installed-p colin-elpa-dependencies))

(defun colin-install-needed-packages ()
  (let ((needed-pkgs (colin-needed-packages)))
    (when needed-pkgs
      (package-refresh-contents)
      (dolist (pkg needed-pkgs)
        (package-install pkg)))))

(provide 'elpa-package-config)
