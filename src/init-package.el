;;; ============================================================================
;;; Package-management related stuff 
;;; ============================================================================

(require 'package)

(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa"        . "http://melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize nil)

(defun require-external (x)
  (unless (package-installed-p x)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install x)
    (require x)))

(provide 'init-package)
