;; =====================
;; Some initial settings
;; =====================

(ignore-errors (kill-buffer "*scratch*"))
(setq inhibit-startup-screen t)
(menu-bar-mode       -1)
(tool-bar-mode       -1)
(auto-fill-mode      -1)
(scroll-bar-mode     -1)

;; No tabs
(setq-default indent-tabs-mode nil)

;; ==============
;; Package system
;; ==============

(require 'package)

(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa"        . "http://melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize nil)

(setq required-packages '(auto-complete
                          gruvbox-theme
                          paredit
                          ido
                          slime
                          highlight-symbol
                          tabbar
                          reverse-im))

(mapc (lambda (x)
        (unless (package-installed-p x)
          (unless package-archive-contents
            (package-refresh-contents))
          (package-install x)))
      required-packages)

;; ========================
;; Some awesome keybindings
;; ========================

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
  (find-file (expand-file-name "~/.emacs.d/init.el")))

(defun load-init-file ()
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))

;; ========================
;; Other cofigs
;; ========================

(require 'reverse-im)
(reverse-im-activate "russian-computer")

(require 'tabbar)
(global-set-key (kbd "C-<next>") 'tabbar-forward-tab)
(global-set-key (kbd "C-<prior>") 'tabbar-backward-tab)
(tabbar-mode)

(require 'highlight-symbol)
(global-set-key (kbd "C-<f3>") 'highlight-symbol)
(global-set-key (kbd "<f3>") 'highlight-symbol-next)
(global-set-key (kbd "S-<f3>") 'highlight-symbol-prev)
(global-set-key (kbd "M-<f3>") 'highlight-symbol-query-replace)

(require 'ido)
(ido-mode t)

(require 'auto-complete)
(ac-config-default)

(require 'paredit)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

;; ==============
;; Theme settings
;; ==============

(load-theme 'gruvbox t)
(set-default-font "Mono-11:antialias=true")

(setq show-paren-delay 0)
(show-paren-mode 1)
(setq show-paren-style 'expression)

(global-linum-mode 1)

;; =============================
;; Slime configuration(for sbcl)
;; =============================

(require 'slime)
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))
