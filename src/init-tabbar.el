;;; ============================================================================
;;; Tabbar configuration
;;; ============================================================================

(require-external 'tabbar)

(global-set-key (kbd "M-]") 'tabbar-forward-tab)
(global-set-key (kbd "M-[") 'tabbar-backward-tab)

(tabbar-mode)

;; for consistent experience
(global-set-key (kbd "M-}") 'next-buffer)
(global-set-key (kbd "M-{") 'next-buffer)

(provide 'init-tabbar)
