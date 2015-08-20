(require 'org)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(require 'bibtex)
(bibtex-set-dialect)

(setq org-default-notes-file "~/Dropbox/notes/captrue.org")
(define-key global-map "\C-cc" 'org-capture)

(add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))

(define-skeleton skel-org-block-elisp
  "Insert an emacs-lisp block"
  ""
  "#+begin_src emacs-lisp\n"
  _ - \n
  "#+end_src")

(define-abbrev org-mode-abbrev-table "elsrc" "" 'skel-org-block-elisp)

(add-hook 'org-mode-hook (lambda ()
                           (calstad-turn-off-electric-indent)))
(provide 'org-config)
