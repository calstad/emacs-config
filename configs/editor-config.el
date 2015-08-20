;; Basic and universal customizations for editing all kinds of text
;; Lots of this taken from the emacs starter kit v2 and emacs prelude

;; Set various configuration variables
(setq sentence-end-double-space nil
      shift-select-mode nil
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 80
      ediff-window-setup-function 'ediff-setup-windows-plain
      diff-switches "-u")

;; Unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)     ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*")  ; don't muck with special buffers

;; Death to the tabs!
(setq-default indent-tabs-mode nil)

;; Delete the selection with a keypress
(delete-selection-mode t)

;; Revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; Hippie expand is dabbrev expand on steroids
;; May want to remove try-expand-line try-expand-list and move
;; try-complete-file-name-partially to the end
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol
                                         try-complete-file-name))

;; Smart indenting and pairing for all
(electric-pair-mode t)
(electric-indent-mode t)
(electric-layout-mode t)

;; show-paren-mode: subtle highlighting of matching parens
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

;; Saveplace remembers your location in a file when saving files
(setq save-place-file (concat  calstad-temporary-file-directory "saveplace"))
;; activate it for all buffers
(setq-default save-place t)
(require 'saveplace)

;; Savehist keeps track of some history
(setq savehist-additional-variables
      ;; search entries
      '(search ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (concat calstad-temporary-file-directory "savehist"))
(savehist-mode t)

(setq recentf-save-file (concat calstad-temporary-file-directory "recentf"))

;; Better navigation of the kill ring
(when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings))
(setq browse-kill-ring-quit-action 'save-and-restore)

;; ido-mode is like magic pixie dust!
;; May want to add ido-default-buffer-method and ido-default-file-method 
(ido-mode t)
(ido-ubiquitous t)
(setq ido-enable-prefix nil
      ido-save-directory-list-file (concat calstad-temporary-file-directory "ido-last")
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

;; Smex adds ido magic to function calls
(setq smex-save-file (concat calstad-temporary-file-directory "semex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; auto-completion in minibuffer
(icomplete-mode +1)

(set-default 'imenu-auto-rescan t)

;; Spell checker
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; Eshell customizations
(setq eshell-cmpl-cycle-completions nil
      eshell-directory-name calstad-temporary-file-directory
      eshell-history-file-name (concat calstad-temporary-file-directory "eshell-history")
      eshell-last-dir-ring-file-name (concat calstad-temporary-file-directory "eshell-lastdir")
      eshell-save-history-on-exit t
      eshell-buffer-shorthand t
      eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

;; HTTP client
(add-to-list 'auto-mode-alist '("\\.http$" . restclient-mode))

;; TRAMP support for vagrant.
(eval-after-load 'tramp
  '(vagrant-tramp-enable))


(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)

(require 'org-config)

(provide 'editor-config)
