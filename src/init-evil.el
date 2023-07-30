;;; ============================================================================
;;; Evil-mode and space-bassed vim keybindings
;;; ============================================================================

;; evil-mode
(require-external 'evil)
(evil-mode 1)
(evil-set-undo-system 'undo-redo)

;; relative line numbering
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; utility functions
(defun open-init-file ()
  (interactive)
  (find-file (expand-file-name (concat user-emacs-directory "init.el"))))

(defun load-init-file ()
  (interactive)
  (load-file (expand-file-name (concat user-emacs-directory "init.el"))))

(defun custom-put-line-bellow ()
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline)
    (goto-char oldpos)))

(defun custom-put-line-above ()
  (interactive)
  (let ((oldpos (point)))
    (beginning-of-line)
    (newline)
    (goto-char (1+ oldpos)))) ; 1+ because newline character was inserted above

;; space-based keybindings
(define-key evil-motion-state-map " " nil)

(define-key evil-motion-state-map (kbd "SPC j") 'evil-window-down)
(define-key evil-motion-state-map (kbd "SPC h") 'evil-window-left)
(define-key evil-motion-state-map (kbd "SPC k") 'evil-window-up)
(define-key evil-motion-state-map (kbd "SPC l") 'evil-window-right)

(define-key evil-normal-state-map " " nil)

(define-key evil-normal-state-map (kbd "SPC w c") 'delete-window)
(define-key evil-normal-state-map (kbd "SPC w o") 'delete-other-windows)
(define-key evil-normal-state-map (kbd "SPC w s") 'split-window-below)
(define-key evil-normal-state-map (kbd "SPC w v") 'split-window-right)
(define-key evil-normal-state-map (kbd "SPC w w") 'other-window)

(define-key evil-normal-state-map (kbd "SPC b k") 'ido-kill-buffer)
(define-key evil-normal-state-map (kbd "SPC b l") 'list-buffers)
(define-key evil-normal-state-map (kbd "SPC b s") 'ido-switch-buffer)
(define-key evil-normal-state-map (kbd "SPC f f") 'ido-find-file)
(define-key evil-normal-state-map (kbd "SPC f s") 'evil-save)

(define-key evil-normal-state-map (kbd "SPC e o") 'open-init-file)
(define-key evil-normal-state-map (kbd "SPC e s") 'load-init-file)

(define-key evil-normal-state-map (kbd "SPC <return>") 'custom-put-line-bellow)
(define-key evil-normal-state-map (kbd "SPC S-<return>") 'custom-put-line-above)

(global-set-key (kbd "M-}") 'next-buffer)
(global-set-key (kbd "M-{") 'next-buffer)

;; recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)

(if (fboundp 'ido-recentf-open)
    (define-key evil-motion-state-map (kbd "SPC f r")
      'ido-recentf-open)
  (define-key evil-motion-state-map (kbd "SPC f r")
    'recentf-open-files))

(provide 'init-evil)
