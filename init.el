(defvar native-comp-deferred-compilation-deny-list nil)
;; Install straight.el to manage packages
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; Make sure packages are in-sync with their pins in straight
(straight-thaw-versions)
;; Have straight immediately load org-mode so the correct version is loaded
(straight-use-package 'org)
(org-babel-load-file (expand-file-name "emacs-config.org" user-emacs-directory))
