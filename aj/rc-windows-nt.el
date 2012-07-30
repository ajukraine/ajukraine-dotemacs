;; Configuration for Emacs NT
(add-site-lisp-dir "ntemacs-cygwin")

(setq exec-path (cons"c:/cygwin/bin" exec-path))
(require 'ntemacs-cygwin)

(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(provide 'rc-windows-nt)
