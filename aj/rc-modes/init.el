;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        Modes customization         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'use-package)

;;; Whitespace mode
(use-package whitespace)

;;; Multi-mode
;; This mode causes the disabling of text color highlight
(use-package multi-mode
  :load-path (expand-site-lisp "multi-mode")
  :disabled t)

;;; CC mode. C++
(use-package cc-mode
  :init 
  (progn
    (defun aj/common-hook ()
      (local-set-key "\C-c:" 'uncomment-region)
      (local-set-key "\C-c;" 'comment-region)
      (linum-mode 1)
      (whitespace-mode 1))

    (defun aj/c++-mode-hook ()
      (setq tab-width 4
	    c-basic-offset 4
	    indent-tabs-mode t)
      (c-set-style "stroustrup")
      (local-set-key [return] 'newline-and-indent))

    (add-hook 'c-mode-common-hook 'aj/common-hook)
    (add-hook 'c++-mode-hook 'aj/c++-mode-hook)))

;;; Lisp
(dolist (lisp-candidate '("sbcl" "clisp"))
  (when (executable-find lisp-candidate)
    (setq inferior-lisp-program lisp-candidate)
    (return)))

;; Activates switch buffer mode
; Now using the IDO mode
;; (iswitchb-mode t)

;;; Markdown-mode
(use-package markdown-mode
  :load-path (expand-site-lisp "markdown-mode")
  :mode ("\\.md$". markdown-mode))


;;; C#, ASPX
;; csharp-mode
(use-package csharp-mode
  :load-path (expand-site-lisp "csharp-mode")
  :mode ("\\.cs$" . csharp-mode))

;; aspx-mode
; Disable
; (autoload 'aspx-mode "aspx-mode" "Major mode for editing ASPX files." t)
(add-to-list 'auto-mode-alist '("\\.aspx$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.ascx$" . nxml-mode))

;;;; Git integration
;;; Magit
(use-package magit
  :load-path (expand-site-lisp "magit")
  :commands magit-status
  :bind ("C-x g" . magit-status))
;;; Git-Emacs
(use-package git-emacs
  :load-path (expand-site-lisp "git-emacs"))

;;;; Mercurial integration
;;; Monky
(use-package monky
  :load-path (expand-site-lisp "monky")
  :commands monky-status
  :bind ("C-x h" . monky-status)
  :init
  (progn
    (setq monky-process-type 'cmdserver)))

;;; Auto-Complete
(use-package auto-complete-config
  :load-path (expand-site-lisp "auto-complete")
  :commands auto-complete-mode
  :bind ("M-`" . auto-complete)
  :config
  (progn
    (ac-config-default)
    (setq ac-auto-start nil)
    (setq ac-use-fuzzy)))

;;; Cursor settings
;; TODO: move to common settings
(if (fboundp 'blink-cursor-mode) 
    (blink-cursor-mode 0))
(set-cursor-color "white")


;;; Slime
(use-package slime
  :load-path (expand-site-lisp "slime")
  :commands slime  
  :config

  (progn
    (add-hook
     'slime-load-hook
     #'(lambda ()
	 (slime-setup 
	  '(slime-fancy
	    slime-repl
	    slime-fuzzy))))
    (setq slime-net-coding-system 'utf-8-unix)
  
    ;; Slime and Auto-Complete
    (use-package ac-slime
      :load-path (expand-site-lisp "ac-slime")
      :init
      (progn
	(add-hook 'slime-mode-hook 'set-up-slime-ac)
	(add-hook 'slime-repl-mode-hook 'set-up-slime-ac))
      :config
      (progn
	(eval-after-load "auto-complete"
	  '(add-to-list 'ac-modes 'slime-repl-mode))))))

;; Uniquify buffers names
(use-package uniquify
  :config
  (setq 
   uniquify-buffer-name-style 'post-forward
   uniquify-after-kill-buffer-p t
   uniquify-ignore-buffers-re "^\\*"))

(load "rc-org")
(load "rc-ido")