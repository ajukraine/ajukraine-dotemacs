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
(load "./rc-modes/init.el")

;; (byte-recompile-directory emacs-root nil)
(provide 'ajukraine)
