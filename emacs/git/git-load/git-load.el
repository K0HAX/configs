(require 'git)

(defvar git-load-default-directory "~/.emacs.d/git/")

(defun git-load (git-load-repository git-load-package-name &optional directory)
"
This function takes a git repository location, package name and optionally a directory, and loads it into Emacs as a package.
Think of it as making all of github into one huge MELPA repo.
"
  (unless directory (setq directory git-load-default-directory))
  (if
      (progn
	(add-to-list 'load-path (concat directory git-load-package-name))
	(load git-load-package-name))(git-repo? (concat directory git-load-package-name))
    (progn
      (let ((git-repo directory))
	(git-clone git-load-repository))
      (add-to-list 'load-path (concat directory git-load-package-name))
      (load git-load-package-name))))
