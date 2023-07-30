;; =====================
;; Some initial settings
;; =====================

;; disable deprecated stuff
(setq inhibit-startup-screen t)
(menu-bar-mode       -1)
(tool-bar-mode       -1)
(auto-fill-mode      -1)
(scroll-bar-mode     -1)
(set-fringe-mode 0)
(global-linum-mode 0)
(setq make-backup-files nil)

;; move it away
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; No tabs
(setq-default indent-tabs-mode nil)
;; (ignore-errors (kill-buffer "*scratch*"))

;; ==============
;; Package system
;; ==============

(require 'package)

(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa"        . "http://melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize nil)

(defun require-and-install (x)
  (unless (package-installed-p x)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install x)
    (require x)))

(require-and-install 'auto-complete)
(require-and-install 'gruvbox-theme)
(require-and-install 'ido)
;; (require-and-install 'slime)
;; (require-and-install 'paredit)
(require-and-install 'highlight-symbol)
(require-and-install 'tabbar)

;; =============================
;; Some common-emacs keybindings
;; =============================

(global-set-key (kbd "C-c i") 'iwb)
(global-set-key (kbd "C-c e o") 'open-init-file)
(global-set-key (kbd "C-c e s") 'load-init-file)

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun open-init-file ()
  (interactive)
  (find-file (expand-file-name (concat user-emacs-directory "init.el"))))

(defun load-init-file ()
  (interactive)
  (load-file (expand-file-name (concat user-emacs-directory "init.el"))))

;; ===================================
;; Which-key for shortcuts suggestions
;; ===================================
(require-and-install 'which-key)
(which-key-mode)

;; ==========================
;; Main customization section
;; ==========================

(global-set-key (kbd "M-}") 'next-buffer)
(global-set-key (kbd "M-{") 'next-buffer)

(require-and-install 'tabbar)

(global-set-key (kbd "M-]") 'tabbar-forward-tab)
(global-set-key (kbd "M-[") 'tabbar-backward-tab)

(tabbar-mode)

;; evil-mode
(require-and-install 'evil)
(evil-mode 1)
(evil-set-undo-system 'undo-redo)

;; relative line numbering
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(define-key evil-motion-state-map " " nil)

(define-key evil-motion-state-map (kbd "SPC w 0") 'delete-window)
(define-key evil-motion-state-map (kbd "SPC w 1") 'delete-other-windows)
(define-key evil-motion-state-map (kbd "SPC w 2") 'split-window-below)
(define-key evil-motion-state-map (kbd "SPC w 3") 'split-window-right)

(define-key evil-motion-state-map (kbd "SPC b k") 'ido-kill-buffer)
(define-key evil-motion-state-map (kbd "SPC b l") 'list-buffers)
(define-key evil-motion-state-map (kbd "SPC b s") 'ido-switch-buffer)
(define-key evil-motion-state-map (kbd "SPC f f") 'ido-find-file)


(define-key evil-motion-state-map (kbd "SPC j") 'evil-window-down)
(define-key evil-motion-state-map (kbd "SPC h") 'evil-window-left)
(define-key evil-motion-state-map (kbd "SPC k") 'evil-window-up)
(define-key evil-motion-state-map (kbd "SPC l") 'evil-window-right)


;; recent files selection
(recentf-mode 1)
(setq recentf-max-menu-items 25)

(define-key evil-motion-state-map (kbd "SPC f r") 'recenf-open-files)

;; ==============
;; Theme settings
;; ==============

(load-theme 'gruvbox t)
(set-frame-font "Mono 12")


;; ========================
;; Other configs
;; ========================

;; (require-and-install 'reverse-im)
;; (reverse-im-activate "ukrainian-computer")


;; (require-and-install 'highlight-symbol)
;; (global-set-key (kbd "C-<f3>") 'highlight-symbol)
;; (global-set-key (kbd "<f3>") 'highlight-symbol-next)
;; (global-set-key (kbd "S-<f3>") 'highlight-symbol-prev)
;; (global-set-key (kbd "M-<f3>") 'highlight-symbol-query-replace)

(require-and-install 'ido)
(ido-mode t)

(require-and-install 'auto-complete)
(ac-config-default)

;; =============================
;; SBCL things
;; =============================

;; (require-and-install 'slime)
;; (setq inferior-lisp-program "/usr/bin/sbcl")
;; (setq slime-contribs '(slime-fancy))

;; (require-and-install 'paredit)
;; (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;; (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;; (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;; (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
;; (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

