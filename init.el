;; Where all the magic starts!

(require 'cl)

;; Add base emacs config root directory to load path
(setq colin-dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path colin-dotfiles-dir)

;; Add needed directories to the load path
(setq colin-configs-dir (concat colin-dotfiles-dir "configs"))
(setq colin-utils-dir (concat colin-dotfiles-dir "utils"))
(setq colin-vendor-dir (concat colin-dotfiles-dir "vendor"))
(add-to-list 'load-path colin-configs-dir)
(add-to-list 'load-path colin-utils-dir)
(add-to-list 'load-path colin-vendor-dir)
(require 'vendor)

;; Initialize package system and install needed packages
(require 'elpa-package-config)
(colin-install-needed-packages)

;; For the love of all that is holy do not litter the file system with
;; backup and autosave files!
(setq user-temporary-file-directory (concat colin-dotfiles-dir ".emacs-tmp-files/"))
(require 'backup-autosave-mgr)
(set-backup-and-autosave-file-locale)

;; Ensure PATH is correctly setup
(if window-system
    (setenv "PATH" (shell-command-to-string "echo $PATH")))

(setq autoload-file (concat colin-dotfiles-dir "loaddefs.el"))
(setq custom-file (concat colin-dotfiles-dir "custom.el"))

;; Load core customizations
(require 'custom-defuns)
(require 'custom-keybindings)
(require 'editor-config)
(require 'ui-config)

;; Load support for programming and markup languages
(require 'programming-config)

;; Keep system type specific customiztions in own files
(if (string-equal system-type "gnu/linux")
    (setq system-type-specific-config (concat colin-dotfiles-dir "linux" ".el"))
  (setq system-type-specific-config (concat colin-dotfiles-dir (symbol-name system-type) ".el")))
(if (file-exists-p system-type-specific-config) (load system-type-specific-config))
