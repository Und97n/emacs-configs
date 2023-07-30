;;; ============================================================================
;;; Common-lisp support
;;; TODO: rewrite keybindings for evil-mode
;;; ============================================================================

;; (require-external 'highlight-symbol)
;; (global-set-key (kbd "C-<f3>") 'highlight-symbol)
;; (global-set-key (kbd "<f3>") 'highlight-symbol-next)
;; (global-set-key (kbd "S-<f3>") 'highlight-symbol-prev)
;; (global-set-key (kbd "M-<f3>") 'highlight-symbol-query-replace)

;; (require-external 'slime)
;; (setq inferior-lisp-program "/usr/bin/sbcl")
;; (setq slime-contribs '(slime-fancy))

;; (require-external 'paredit)
;; (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;; (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;; (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;; (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
;; (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

(provide 'init-cl)
