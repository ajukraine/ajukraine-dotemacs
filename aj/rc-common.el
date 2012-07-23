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

(if (fboundp 'aj/toggle-fullscreen)
    (global-set-key (kbd "<f11>") 'aj/toggle-fullscreen))
(if (fboundp 'aj/fullscreen)
    (global-set-key (kbd "S-<f11>") 'aj/fullscreen))

;;; Customize that emacs 'bell' sound
(defun aj/ring-bell-function ()
  (unless (memq this-command
		'(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit
				mwheel-scroll down up next-line previous-line
				backward-char forward-char))
    (ding)))
(setq ring-bell-function 'aj/ring-bell-function)

(provide 'rc-common)
