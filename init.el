;; Install straight.el to manage packages
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; Have straight immediately load org-mode so the correct version is loaded
;; Right now we are using the built in version due to issues in getting the newer versions to work
(straight-use-package 'org-plus-contrib)
(org-babel-load-file (expand-file-name "emacs-config.org" user-emacs-directory))
