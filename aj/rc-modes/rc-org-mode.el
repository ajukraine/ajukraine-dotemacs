;;; Org mode

(use-package org-mode
  :mode ("\\.org\\'" . org-mode)
  :bind (("\C-cl" . org-store-link)
	 ("\C-cc" . org-capture)
	 ("\C-ca" . org-agenda)
	 ("\C-cb" . org-iswitchb))
  :congif
  (progn
    ;; Configure todo's keywords
    (setq org-todo-keywords
	  '((sequence "TODO(t)" "REOPEN(r@/!)" "|" "DONE(d!)")
	    (sequence "BUG(b)" "|" "FIXED(f!)")))
    (setq org-log-done 'note)
    ))