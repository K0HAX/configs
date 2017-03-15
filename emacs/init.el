;; `org-babel-load-file` increases startup time, so only do it if necessary.
;; To reload any config changes, delete config.el and restart emacs.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(if (file-newer-than-file-p "~/.emacs.d/config.el" "~/.emacs.d/config.elc")
    (byte-compile-file "~/.emacs.d/config.el"))

(if (file-exists-p "~/.emacs.d/config.el")
    (progn
      (if (file-newer-than-file-p "~/.emacs.d/config.el" "~/.emacs.d/config.elc")
	  (byte-compile-file "~/.emacs.d/config.el"))
      (load-file "~/.emacs.d/config.elc"))
  (org-babel-load-file "~/.emacs.d/config.org"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(trailing-whitespace ((t (:background "dim gray")))))
