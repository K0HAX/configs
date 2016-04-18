;; Automatic package install
(setq package-list '(
		     evil
		     evil-leader
		     evil-org
		     git-commit
		     smex
		     ledger-mode
		     magit
		     magit-popup
		     mediawiki
		     org
		     org-gcal
		     org-bullets
		     powerline
		     powerline-evil
		     twittering-mode
		     use-package
		     highlight-parentheses))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
      (package-install package)))

(setq ring-bell-function 'ignore)

;; My Packages
(require 'powerline)
(powerline-default-theme)

(load "~/.emacs.d/private/highlight-parentheses.el")
(load "~/.emacs.d/private/org-bullets.el")
(load "~/.emacs.d/private/key-bindings.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
 '(ledger-reports
   (quote
    (("register" "ledger ")
     ("bal" "ledger -f %(ledger-file) bal")
     ("reg" "ledger -f %(ledger-file) reg")
     ("payee" "ledger -f %(ledger-file) reg @%(payee)")
     ("account" "ledger -f %(ledger-file) reg %(account)"))))
 '(newsticker-url-list-defaults
   (quote
    (("NY Times" "http://partners.userland.com/nytRss/nytHomepage.xml")
     ("The Register" "http://www.theregister.co.uk/tonys/slashdot.rdf")
     ("slashdot" "http://slashdot.org/index.rss" nil 3600))))
 '(org-agenda-files (quote ("~/Documents/todo.org")))
 '(twittering-use-master-password t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Enable evil mode LAST.
(require 'evil)
(evil-mode 1)

;; Evil Mods
(define-key evil-motion-state-map ";" 'smex)
(define-key evil-motion-state-map ":" 'evil-ex)

;; Ledger Mode
(use-package ledger-mode
	     :ensure t
	     :init
	     (setq ledger-clear-whole-transactions 1)

	     :config
	     (add-to-list 'evil-emacs-state-modes 'ledger-report-mode)
	     :mode "\\.ldg\\'")
