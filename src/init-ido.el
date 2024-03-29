;;; ============================================================================
;;; Ido-related stuff
;;; ============================================================================

(require-external 'ido)
(ido-mode t)

;; Display results vertically
(require-external 'ido-vertical-mode)
(ido-vertical-mode)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-case-fold t ;; ignore case
      ido-auto-merge-work-directories-length -1 ;; disable auto-merge (confusing)
      ido-create-new-buffer 'always ;; create new files easily
      ido-use-filename-at-point nil ;; don't try to be smart about what I want
      ido-everywhere t
      )

;; I like visual matching (colors)
(setq ido-use-faces t)

;; Ido buffer intuitive navigation
(add-hook 'ido-setup-hook
          '(lambda ()
             (define-key ido-completion-map "\C-h" 'ido-delete-backward-updir)
             (define-key ido-completion-map "\C-n" 'ido-next-match)
             (define-key ido-completion-map "\C-f" 'ido-next-match)
             (define-key ido-completion-map "\C-p" 'ido-prev-match)
             (define-key ido-completion-map "\C-b" 'ido-prev-match)
             (define-key ido-completion-map " " 'ido-exit-minibuffer)
             ))

;; Ignore .DS_Store files with ido mode
(add-to-list 'ido-ignore-files "\\.DS_Store")

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(provide 'init-ido)
