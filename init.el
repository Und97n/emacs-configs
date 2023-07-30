;;; ============================================================================
;;; Base init file. All complicated things should be done in a separate 
;;; init-files under `src' directory.
;;; ============================================================================

(add-to-list 'load-path (concat user-emacs-directory "src/"))

(require 'init-base)
(require 'init-package)
(require 'init-splash)
(require 'init-ido)
(require 'init-tabbar)
(require 'init-evil) ; may be disabled if needed
(require 'init-appearance)
