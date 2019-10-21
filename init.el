; (add-to-list 'load-path "/home/zizitop/.emacs.d/ergoemacs-mode")
; (require 'ergoemacs-mode)


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq show-paren-style 'expression)
(show-paren-mode 2)

;; Disable GUI components
(tooltip-mode      -1)
; (menu-bar-mode     -1) ;; отключаем графическое меню
(tool-bar-mode     -1) ;; отключаем tool-bar
(scroll-bar-mode   -1) ;; отключаем полосу прокрутки
(blink-cursor-mode -1) ;; курсор не мигает
(setq use-dialog-box     nil) ;; никаких графических диалогов и окон - все через минибуфер
(setq redisplay-dont-pause t)  ;; лучшая отрисовка буфера
(setq ring-bell-function 'ignore) ;; отключить звуковой сигнал

;; Display file size/time in mode-line
(setq display-time-24hr-format t) ;; 24-часовой временной формат в mode-line
(display-time-mode             t) ;; показывать часы в mode-line
(size-indication-mode          t) ;; размер файла в %-ах

;; Line wrapping
(setq word-wrap          t) ;; переносить по словам
(global-visual-line-mode t)

;; Start window size
(when (window-system)
    (set-frame-size (selected-frame) 100 50))

;; Clipboard settings
(setq x-select-enable-clipboard t)

(setq make-backup-files         nil) ; Don't want any backup files
(setq auto-save-list-file-name  nil) ; Don't want any .saves files
(setq auto-save-default         nil) ; Don't want any auto saving


(add-to-list 'load-path "/home/zizitop/.emacs.d/loads/")

;; http://code.google.com/p/dea/source/browse/trunk/my-lisps/linum%2B.el
(require 'linum+)
(setq linum-format "%d ")
(global-linum-mode 1)

;; Inhibit startup/splash screen
(setq inhibit-splash-screen   t)
(setq ingibit-startup-message t) ;; экран приветствия можно вызвать комбинацией C-h C-a

;; built-in
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)


;; built-in
(require 'bs)
(setq bs-configurations
      '(("files" "^\\*scratch\\*" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)))

(global-set-key (kbd "<f2>") 'bs-show)

;; make return key also do indent, for current buffer only
(electric-indent-local-mode 1)

(cua-mode 1)

;; Electric-modes settings
(electric-pair-mode    1) ;; автозакрытие {},[],() с переводом курсора внутрь скобок

;; http://www.emacswiki.org/emacs/AutoComplete
(add-to-list 'load-path "/home/zizitop/.emacs.d/auto-complete")
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "/home/zizitop/.emacs.d/auto-complete/dict")

;; http://www.emacswiki.org/emacs/SrSpeedbar
(require 'sr-speedbar)
(global-set-key (kbd "<f12>") 'sr-speedbar-toggle)


;; http://www.emacswiki.org/emacs/Yasnippet
(add-to-list 'load-path "/home/zizitop/.emacs.d/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)
(yas/load-directory "/home/zizitop/.emacs.d/yasnippet/snippets")

;; http://www.emacswiki.org/emacs/ColorTheme
;; Tools -> Color themes
(add-to-list 'load-path "/home/zizitop/.emacs.d/color-theme/")
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)

(color-theme-robin-hood)
(color-theme-classic)
(color-theme-select)

;; Options -> Set default font
(add-to-list 'default-frame-alist '(font . "Consolas-10"))
(set-default-font "Consolas-10")


;; ;; KEYS
;; (global-set-key (kbd "M-w") 'previous-line)
;; (global-set-key (kbd "M-a") 'backward-char)
;; (global-set-key (kbd "M-s") 'next-line)
;; (global-set-key (kbd "M-d") 'forward-char)

;; (global-set-key (kbd "M-C-q") 'backward-word)
;; (global-set-key (kbd "M-C-e") 'forward-word)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (wombat)))
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa-stable" . "http://stable.melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (w3 ghc stack-mode haskell-mode slime expand-region paredit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'package)

(add-to-list 'package-archives
         '("MELPA" . "http://melpa.milkbox.net/packages/") t)

(defun setup-lisp-mode ()
  (require 'paredit)
  (paredit-mode))

; (add-hook 'lisp-mode-hook
;       'setup-lisp-mode)

; (add-hook 'emacs-lisp-mode-hook
;       'setup-lisp-mode)
;; Set your lisp system and, optionally, some contribs
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))
(put 'erase-buffer 'disabled nil)
   (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
    (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
    (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; ----------------------------------------------------------------------
;; HASKELL
(require 'haskell-mode)
;; (require 'intero)
;; (add-hook 'haskell-mode-hook 'intero-mode)
;; (require 'flycheck)
;; (autoload 'ghc-init "ghc" nil t)
;; (autoload 'ghc-debug "ghc" nil t)
;; (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; (setq flycheck-check-syntax-automatically '(save new-line))
;; (flycheck-add-next-checker 'intero '(warning . haskell-hlint))
;; make these keys behave like normal browser
;; (define-key xwidget-webkit-mode-map [mouse-4] 'xwidget-webkit-scroll-down)
;; (define-key xwidget-webkit-mode-map [mouse-5] 'xwidget-webkit-scroll-up)
;; (define-key xwidget-webkit-mode-map (kbd "<up>") 'xwidget-webkit-scroll-down)
;; (define-key xwidget-webkit-mode-map (kbd "<down>") 'xwidget-webkit-scroll-up)
;; (define-key xwidget-webkit-mode-map (kbd "M-w") 'xwidget-webkit-copy-selection-as-kill)
;; (define-key xwidget-webkit-mode-map (kbd "C-c") 'xwidget-webkit-copy-selection-as-kill)

;; adapt webkit according to window configuration chagne automatically
;; without this hook, every time you change your window configuration,
;; you must press 'a' to adapt webkit content to new window size
(add-hook 'window-configuration-change-hook (lambda ()
			   (when (equal major-mode 'xwidget-webkit-mode)
			     (xwidget-webkit-adjust-size-dispatch))))

;; by default, xwidget reuses previous xwidget window,
;; thus overriding your current website, unless a prefix argument
;; is supplied
;;
;; This function always opens a new website in a new window
(defun xwidget-browse-url-no-reuse (url &optional sessoin)
  (interactive (progn
                 (require 'browse-url)
                 (browse-url-interactive-arg "xwidget-webkit URL: "
                                             )))
  (xwidget-webkit-browse-url url t))

;; make xwidget default browser
(setq browse-url-browser-function (lambda (url session)
				    (other-window 1)
				    (xwidget-browse-url-no-reuse url)))
