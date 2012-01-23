;; Main config file.

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; Maybe I do like the scroll bar
;; (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Some modes need to call stuff on the exec-path
(push "/usr/local/bin" exec-path)

;; Load path etc.
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)
;; Add directories to the load path
(dolist (dirs '("colin" "vendor"))
  (add-to-list 'load-path (concat dotfiles-dir dirs)))

;; Load up ELPA, the package manager
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
;; Create a list to add needed packages to.
(defvar colin-required-packages '())

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))
;; config changes made through the customize UI will be store here
(setq custom-file (concat dotfiles-dir "custom.el"))

;; Keep system type specific customiztions in own files
(if (string-equal system-type "gnu/linux")
    (setq system-type-specific-config (concat dotfiles-dir "linux" ".el"))
  (setq system-type-specific-config (concat dotfiles-dir (symbol-name system-type) ".el")))
(if (file-exists-p system-type-specific-config) (load system-type-specific-config))
