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

(setq
 windows-p (eq system-type 'windows-nt)
 linux-p (eq system-type 'gnu/linux))

(add-to-list 'load-path (join-path emacs-root "aj"))

(byte-recompile-directory emacs-root)
(require 'ajukraine)
