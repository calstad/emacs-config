;; Where all the magic starts!

(require 'cl)

;; Add base emacs config root directory to load path
(setq calstad-dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Add needed directories to the load path
(setq calstad-configs-dir (concat calstad-dotfiles-dir "configs"))
(setq calstad-utils-dir (concat calstad-dotfiles-dir "utils"))
(add-to-list 'load-path calstad-configs-dir)
(add-to-list 'load-path calstad-utils-dir)

;; Initialize package system and install needed packages
(require 'elpa-package-config)
(calstad-install-needed-packages)

;; For the love of all that is holy do not litter the file system with
;; backup and autosave files!
(setq calstad-temporary-file-directory (concat calstad-dotfiles-dir ".emacs-tmp-files/"))
(require 'backup-autosave-mgr)
(set-backup-and-autosave-file-locale)

;; Ensure PATH is correctly setup
(exec-path-from-shell-initialize)

(setq autoload-file (concat calstad-dotfiles-dir "loaddefs.el"))
(setq custom-file (concat calstad-dotfiles-dir "custom.el"))
(load custom-file t)

;; Load core customizations
(require 'custom-defuns)
(require 'custom-keybindings)
(require 'editor-config)
(require 'ui-config)

;; Load support for programming and markup languages
(require 'programming-config)

;; Keep system type specific customiztions in own files
(if (string-equal system-type "gnu/linux")
    (setq system-type-specific-config (concat calstad-dotfiles-dir "linux" ".el"))
  (setq system-type-specific-config (concat calstad-dotfiles-dir (symbol-name system-type) ".el")))
(if (file-exists-p system-type-specific-config) (load system-type-specific-config))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
