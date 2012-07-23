;; Configuration for Emacs NT
(add-site-lisp-dir "ntemacs-cygwin")

(setq exec-path (cons"c:/cygwin/bin" exec-path))

(require 'ntemacs-cygwin)
(provide 'rc-windows-nt)
