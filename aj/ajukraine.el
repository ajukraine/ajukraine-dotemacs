;; Utilize 'use-packge for managing emacs configuration
(add-site-lisp-dir "use-package")
(require 'use-package)
(eval-when-compile
  (setq use-package-verbose (null byte-compile-current-file)))


;; Load OS specific configurations
(cond
 (linux-p (require 'rc-gnu-linux))
 (windows-p (require 'rc-windows-nt)))

;; Load common configurations
(require 'rc-common)

;; Load modes customization
(add-to-list 'load-path (join-path emacs-root "aj" "rc-modes"))
(load "rc-modes/init")

(defun list-to-string (list)
  (let ((result ""))
    (loop for str in list 
	  do (setq result (concat result str)))
    result))

;; (byte-recompile-directory emacs-root nil)
(provide 'ajukraine)
