;;; ============================================================================
;;; Appearance-related stuff (theme, font, key suggestions, etc.)
;;; ============================================================================

(require-external 'gruvbox-theme)
(load-theme 'gruvbox t)
(set-frame-font "JetBrains Mono 18")

;; which-key for shortcuts suggestions
(require-external 'which-key)
(which-key-mode)

;; In order to make all keybindings work under differend keyboard layouts
(require-external 'reverse-im)
(reverse-im-add-input-method "ukrainian-computer")
(reverse-im-add-input-method "russian-computer")
(reverse-im-mode t)

;; auto-complete
(require-external 'auto-complete)
(ac-config-default)

(provide 'init-appearance)
