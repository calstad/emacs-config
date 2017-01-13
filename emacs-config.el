(setq user-full-name "Colin Alstad")
(setq user-mail-address "colin.alstad@gmail.com")

(require 'cl)

(setq mac-command-modifier 'meta)

(setq ring-bell-function 'ignore)

(if window-system
    (setq default-frame-alist '((font . "-*-Consolas-medium-r-normal--16-0-72-72-m-0-iso10646-1"))))

(defvar calstad/packages '(auctex
                           company
                           elisp-slime-nav
                           elpy
                           haskell-mode
                           helm
                           intero
                           magit
                           org
                           paredit
                           yasnippet
                           zenburn-theme)
  "Default packages")

(defun calstad/packages-installed-p ()
  (loop for pkg in calstad/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(unless (calstad/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg calstad/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(setq custom-file "~/.emacs_custom.el")
(load custom-file)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)     ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*")  ; don't muck with special buffers

(show-paren-mode t)
(setq show-paren-style 'parenthesis)
(electric-pair-mode)

(setq make-backup-files nil)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq auto-save-list-file-prefix nil)

(fset 'yes-or-no-p 'y-or-n-p)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)       ; dont need mouse tootips!
  (mouse-wheel-mode t)    ; stupid mice
  (blink-cursor-mode -1)  ; about to give me seizures
  (tool-bar-mode -1))

(load-theme 'zenburn t)

(setq visible-bell t)

(set-default 'indicate-empty-lines t)

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(delete-selection-mode t)
(transient-mark-mode t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(defun calstad/unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

(defun calstad/swap-windows ()
  (interactive)
  (if (/= (count-windows) 2)
      (message "You need exactly 2 windows to do this.")
    (let* ((w1 (first (window-list)))
	   (w2 (second (window-list)))
	   (b1 (window-buffer w1))
	   (b2 (window-buffer w2))
	   (s1 (window-start w1))
	   (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)))
  (other-window 1))

(global-set-key (kbd "C-c s") 'calstad/swap-windows)

(defun calstad/kill-other-buffers ()
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

(defun calstad/delete-file-and-buffer ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (delete-file filename)
      (message "Deleted file %s" filename)))
  (kill-buffer))

(global-set-key (kbd "C-c C-k") 'calstad/delete-file-and-buffer)

(add-hook 'after-init-hook 'global-company-mode)

(require 'helm)
(helm-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(helm-autoresize-mode 1)

(setenv "IPY_TEST_SIMPLE_PROMPT" "1")

(elpy-enable)

(elpy-use-ipython)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

(add-hook 'haskell-mode-hook 'intero-mode)

(add-hook 'TeX-mode-hook
	  '(lambda ()
	     (define-key LaTeX-mode-map (kbd "$") 'self-insert-command)))

(setq TeX-electric-sub-and-superscript 't)

(setq magit-last-seen-setup-instructions "1.4.0")

(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-src-fontify-natively t)

(setq org-todo-keywords
 '((sequence "TODO" "IN-PROGRESS" "|" "DONE")))

(setq org-log-done 'time)

(add-to-list 'org-modules 'org-habit)

(put 'dired-find-alternate-file 'disabled nil)

(require 'yasnippet)
(yas-global-mode 1)
