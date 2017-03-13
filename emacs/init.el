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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ledger-reports
   (quote
    (("register" "ledger ")
     ("bal" "ledger -f %(ledger-file) bal Assets Liabilities")
     ("bigbal" "ledger -f %(ledger-file) bal")
     ("reg" "ledger -f %(ledger-file) reg")
     ("payee" "ledger -f %(ledger-file) reg @%(payee)")
     ("account" "ledger -f %(ledger-file) reg %(account)"))))
 '(newsticker-url-list-defaults
   (quote
    (("NY Times" "http://partners.userland.com/nytRss/nytHomepage.xml")
     ("The Register" "http://www.theregister.co.uk/tonys/slashdot.rdf")
     ("slashdot" "http://slashdot.org/index.rss" nil 3600))))
 '(package-selected-packages
   (quote
    (matrix-client mclient web-mode w3m use-package twittering-mode smex rainbow-delimiters powerline-evil ox-twbs org-bullets multi-term magit ledger-mode json-mode htmlize git exec-path-from-shell erc-youtube erc-social-graph erc-image erc-hl-nicks erc-crypt erc-colorize emms-info-mediainfo dtrt-indent company color-theme calfw bbdb)))
 '(send-mail-function (quote smtpmail-send-it))
 '(twittering-use-master-password t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(trailing-whitespace ((t (:background "dim gray")))))
