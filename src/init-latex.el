;;; ============================================================================
;;; LaTeX-related stuff
;;; ============================================================================

(require-external 'auctex)
(require-external 'latex-preview-pane)

(setq TeX-engine 'xetex)

(add-hook 'LaTeX-mode-hook
          (lambda ()
	    (push (list 'output-pdf "Zathura")
		  TeX-view-program-selection)))

(provide 'init-latex)
