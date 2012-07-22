;;;; Emacs configuration
;;;; @author Bohdan Makohin
;;;; Emacs 24.1.1

(require 'cl)

(defun normalize-slashes (pathname)
  "Reverse the Windows-backslashes in PATHNAME to be Unix-slashes; get rid of doubles"
  (replace-regexp-in-string "//" "/" (replace-regexp-in-string "\\\\" "/" pathname)))

(defun join-path (&rest args)
  (normalize-slashes
   (mapconcat 'identity args "/")))

(defvar emacs-root
  (let ((candidates
	 (list (join-path (getenv "HOME") "emacs")
	       (getenv "HOME"))))
    (reduce (lambda (old-path new-path)
	      (or old-path
		  (and (file-exists-p (join-path new-path ".emacs"))
		       new-path)))
	    candidates
	    :initial-value nil))
  "The root folder for emacs config.")

(defun add-site-lisp-dir (dir)
  (add-to-list 'load-path
	       (let
		   ((site-path (join-path "/usr/share/emacs/site-lisp" dir))
		    (local-path (join-path emacs-root "site-lisp" dir)))
		 (or
		  (and (file-exists-p site-path) site-path)
		  (and (file-exists-p local-path) local-path)
		  (and t (error "site-lisp directory '%s' not found" dir))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Common settings ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Navigate between windows, using <Meta>+<Arrow>
(windmove-default-keybindings 'meta)

;; Simulate Windows-like behaviour
;; Copy to system clipboard
(setq-default x-select-enable-clipboard t)
;; Do not create backup files '*~' on each modification
(setq-default make-backup-files nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-splash-screen t)
 ;; Turn off 'tool bar'
 '(tool-bar-mode nil)
 ;; Turn off 'scroll bar'
 '(scroll-bar-mode nil)
 '(browse-url-browser-function 'browse-url-default-browser)
 ;; Enable CUA keys (Cut/Copy/Paste with C-x/C-c/C-v)
 '(cua-mode t nil (cua-base))
 '(show-paren-mode t))

;; Set font/appearance settings
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default 
    ((t (
	 :inherit nil 
		  :stipple nil 
		  :background "black" 
		  :foreground "white" 
		  :inverse-video nil 
		  :box nil 
		  :strike-through nil 
		  :overline nil 
		  :underline nil 
		  :slant normal 
		  :weight thin
		  :height 98 
		  :width normal 
		  :foundry "unknown" 
		  :family "DejaVu Sans Mono"))))
 '(show-paren-match ((((class color) (background dark)) (:foreground "black" :background "green")))))

;; Set the conservative scrolling
(setq scroll-conservatively 50)
(setq scroll-margin 4)

;; Sets encoding of files
(setq file-name-coding-system 'utf-8)

;; Saves The Last Session State
(desktop-save-mode t)

;;; Interactive 'FullScrenn' toggle solution
;; Dosn't work on Windows
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
			 (if (equal 'fullboth current-value)
			     (if (boundp 'old-fullscreen) old-fullscreen nil)
			   (progn (setq old-fullscreen current-value)
				  'fullboth)))))

(global-set-key (kbd "<f11>") 'toggle-fullscreen)

(defun fullscreen (&optional f)
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))
(global-set-key (kbd "S-<f11>") 'fullscreen)


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
