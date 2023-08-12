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
(defun custom-open-init-file ()
  (interactive)
  (find-file (expand-file-name (concat user-emacs-directory "init.el"))))

(defun custom-load-init-file ()
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

(defun custom-forward-kill-whitespace-or-word ()
  "If `point' is followed by whitespace kill that.
Otherwise call `kill-word'"
  (interactive)
  (if (looking-at "[ \t\n]")
      (let ((pos (point)))
        (re-search-forward "[^ \t\n]" nil t)
        (backward-char)
        (kill-region pos (point)))
    (kill-word 1)))

(global-set-key (kbd "M-}") 'next-buffer)
(global-set-key (kbd "M-{") 'next-buffer)

;; recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; space-based keybindings
(define-key evil-motion-state-map " " nil)

(define-key evil-insert-state-map (kbd "C-f") 'custom-forward-kill-whitespace-or-word)

(defun define-evil-key (key fnc)
  (define-key evil-normal-state-map key fnc))

(define-evil-key " " nil)

(define-evil-key (kbd "SPC w c") 'delete-window)
(define-evil-key (kbd "SPC w o") 'delete-other-windows)
(define-evil-key (kbd "SPC w s") 'split-window-below)
(define-evil-key (kbd "SPC w v") 'split-window-right)
(define-evil-key (kbd "SPC w w") 'other-window)

(define-evil-key (kbd "SPC w j") 'evil-window-down)
(define-evil-key (kbd "SPC w h") 'evil-window-left)
(define-evil-key (kbd "SPC w k") 'evil-window-up)
(define-evil-key (kbd "SPC w l") 'evil-window-right)

(define-evil-key (kbd "SPC b k") 'ido-kill-buffer)
(define-evil-key (kbd "SPC b l") 'list-buffers)
(define-evil-key (kbd "SPC b s") 'ido-switch-buffer)
(define-evil-key (kbd "SPC f f") 'ido-find-file)
(define-evil-key (kbd "SPC f s") 'save-buffer)
(define-evil-key (kbd "SPC f r") 'ido-recentf-open)

(define-evil-key (kbd "SPC e o") 'custom-open-init-file)
(define-evil-key (kbd "SPC e s") 'custom-load-init-file)

(define-evil-key (kbd "SPC <return>") 'custom-put-line-bellow)
(define-evil-key (kbd "SPC S-<return>") 'custom-put-line-above)

;; LaTeX integration
(define-evil-key (kbd "SPC l e") 'LaTeX-environment)
(define-evil-key (kbd "SPC l s") 'LaTeX-section)
(define-evil-key (kbd "SPC l c") 'TeX-command-master)
(define-evil-key (kbd "SPC l v") 'TeX-view)

(provide 'init-evil)
