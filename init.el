;; Where all the magic starts!

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; Maybe I do like the scroll bar
;; (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Add base emacs config root directory to load path
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)

;; Add needed directories to the load path
(setq customization-dir (concat dotfiles-dir "customizations"))
(setq utils-dir (concat dotfiles-dir "utils"))
(setq vendor-dir (concat dotfiles-dir "vendor"))
(add-to-list 'load-path customization-dir)
(add-to-list 'load-path utils-dir)
(add-to-list 'load-path vendor-dir)

;; For the love of all that is holy do not litter the file system with
;; backup and autosave files!
(setq user-temporary-file-directory (concat dotfiles-dir ".emacs-tmp-files/"))
(require 'backup-autosave-mgr)
(set-backup-and-autosave-file-locale)

;; Ensure PATH is correctly setup
(require 'init-exec-path)
(if window-system (set-exec-path-from-shell-PATH))

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq custom-file (concat dotfiles-dir "custom.el"))

;; Load up ELPA, the package manager with marmalade
(setq package-user-dir (concat dotfiles-dir "elpa"))
(require 'elpa-utils)
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Load core customizations
(require 'my-editor)
(require 'my-ui)

;; Load support for programming and markup languages
(require 'my-magit)

;; Keep system type specific customiztions in own files
(if (string-equal system-type "gnu/linux")
    (setq system-type-specific-config (concat dotfiles-dir "linux" ".el"))
  (setq system-type-specific-config (concat dotfiles-dir (symbol-name system-type) ".el")))
(if (file-exists-p system-type-specific-config) (load system-type-specific-config))
