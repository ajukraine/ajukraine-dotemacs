;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        Modes customization         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CC mode. C++
(defun aj/common-hook ()
  (local-set-key "\C-c:" 'uncomment-region)
  (local-set-key "\C-c;" 'comment-region))

(defun aj/c++-mode-hook ()
  (setq tab-width 4
	c-basic-offset 4
	indent-tabs-mode t)
  (c-set-style "stroustrup")
  (local-set-key [return] 'newline-and-indent))

(add-hook 'c-mode-common-hook 'aj/common-hook)
(add-hook 'c++-mode-hook 'aj/c++-mode-hook)

;;; Lisp
(setq inferior-lisp-program "sbcl")

;; Activates switch buffer mode
(iswitchb-mode t)

;;; Markdown-mode
(add-site-lisp-dir "markdown-mode")
(autoload 'markdown-mode "markdown-mode.el" 
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md". markdown-mode) auto-mode-alist))


;;;; Git integration
;;; Magit
(add-site-lisp-dir "magit")
(require 'magit)
;;; Git-Emacs
(add-site-lisp-dir "git-emacs")
(require 'git-emacs)

;;;; Mercurial integration
;;; Monky
(add-site-lisp-dir "monky")
(require 'monky)
(setq monky-process-type 'cmdserver)

;;; Auto-Complete
(add-site-lisp-dir "auto-complete")
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-start nil)
(define-key ac-mode-map (kbd "M-`") 'auto-complete)
(setq ac-use-fuzzy)


;;; Slime
(add-site-lisp-dir "slime")
(require 'slime)
(slime-setup '(slime-fancy))


;;; Slime and Auto-Complete
(add-site-lisp-dir "ac-slime")
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
   '(add-to-list 'ac-modes 'slime-repl-mode))
