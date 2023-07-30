;;; ============================================================================
;;; Basic initialization (before package-system setup).
;;; ============================================================================

;; disable deprecated stuff
(menu-bar-mode       -1)
(tool-bar-mode       -1)
(auto-fill-mode      -1)
(scroll-bar-mode     -1)
(set-fringe-mode 0)
(global-linum-mode 0)
;; (setq inhibit-startup-screen t)

(setq make-backup-files nil)

;; move it away
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; No tabs
(setq-default indent-tabs-mode nil)
;; No scratch
;;(ignore-errors (kill-buffer "*scratch*"))

(when (window-system)
  (set-frame-height (selected-frame) 80)
  (set-frame-width (selected-frame) 90))

(provide 'init-base)
