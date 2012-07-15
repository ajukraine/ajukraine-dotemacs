;;;; Emacs configuration
;;;; @author Bohdan Makohin
;;;; Emacs 24.1.1

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


;; Activates switch buffer mode
(iswitchb-mode t)

;;; Markdown-mode
(add-to-list 'load-path "~/emacs/site-lisp/markdown-mode")
(autoload 'markdown-mode "markdown-mode.el" 
  "Major mode for editing Markdown files" t)


;;; Magit
(add-to-list 'load-path "~/emacs/site-lisp/magit")
(require 'magit)
