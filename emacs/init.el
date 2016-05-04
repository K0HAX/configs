;; Automatic package install
(add-to-list 'load-path "~/.emacs.d/git/orgmode-mediawiki/")
(setq package-list '(
		     use-package))

;; Load package installers
(load "~/.emacs.d/private/installers/highlight-parentheses.el")
(load "~/.emacs.d/private/installers/erc-mods.el")
(load "~/.emacs.d/private/installers/twitter-mode.el")
(load "~/.emacs.d/private/installers/powerline-evil.el")
(load "~/.emacs.d/private/installers/org.el")
(load "~/.emacs.d/private/installers/mediawiki.el")
(load "~/.emacs.d/private/installers/magit.el")
(load "~/.emacs.d/private/installers/ledger.el")
(load "~/.emacs.d/private/installers/smex.el")
(load "~/.emacs.d/private/installers/git.el")
(load "~/.emacs.d/private/installers/evil.el")
(load "~/.emacs.d/private/installers/emms.el")
(load "~/.emacs.d/private/installers/s.el")
(load "~/.emacs.d/private/installers/theme-solarized.el")
(load "~/.emacs.d/private/installers/w3m.el")

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
(scroll-bar-mode -1)

(require 'ox-mediawiki)

(load "~/.emacs.d/private/restart-emacs.el")
(load "~/.emacs.d/private/org-bullets.el")
(load "~/.emacs.d/private/erc-freenode.el")
(load "~/.emacs.d/private/twitter-mode.el")
(load "~/.emacs.d/private/highlight-parentheses.el")
(load "~/.emacs.d/private/emms.el")
(load "~/.emacs.d/private/w3m.el")
(load "~/.emacs.d/private/theme-solarized.el")
(load "~/.emacs.d/private/mediawiki.el")
(load "~/.emacs.d/private/gnus.el")

;; Powerline
(powerline-default-theme)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
 '(send-mail-function (quote smtpmail-send-it))
 '(server-mode t)
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
(load "~/.emacs.d/private/key-bindings.el")
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
