;;; ============================================================================
;;; Appearance-related stuff (theme, font)
;;; ============================================================================

(require-external 'gruvbox-theme)
(load-theme 'gruvbox t)

(setq initial-frame-alist '((font . "JetBrains Mono 16")))
(setq default-frame-alist '((font . "JetBrains Mono 16")))

(provide 'init-appearance)
