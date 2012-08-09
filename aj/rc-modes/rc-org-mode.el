;;; Org mode

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (("\C-cl" . org-store-link)
	 ("\C-cc" . org-capture)
	 ("\C-ca" . org-agenda)
	 ("\C-cb" . org-iswitchb))
  :congif
  (progn
    ;; Configure todo's keywords
    (setq 
     org-todo-keywords
     '((sequence "TODO(t)" "REOPEN(r@/!)" "|" "DONE(d!)")
       (sequence "BUG(b)" "|" "FIXED(f!)"))

     org-log-done 'note
     ;; org-empty-line-terminates-plain-lists t
     )

    ;; (setq org-startup-indented t)
    ))