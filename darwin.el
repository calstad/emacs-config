;; Set meta to apple key
(setq mac-command-modifier 'meta)

;; Keep visual bell from displaying black box in cocoa emacs
(setq ring-bell-function 'ignore)

;; Set font
(if window-system
    (setq default-frame-alist '((font . "-apple-inconsolata-medium-r-normal--14-0-72-72-m-0-iso10646-1"))))
