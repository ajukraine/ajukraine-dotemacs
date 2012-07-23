
;; Load OS specific configurations
(cond
 (linux-p (require 'rc-gnu-linux))
 (windows-p (require 'rc-windows-nt)))

;; Load common configurations
(require 'rc-common)

;; Load modes customization
(load "./rc-modes/init.el")

(provide 'ajukraine)
