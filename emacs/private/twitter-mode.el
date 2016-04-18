(eval-after-load 'twittering-mode
  '(progn
     (if (executable-find "convert")
	 (setq twittering-convert-fix-size 32))
     (if (executable-find "gzip")
	 (setq twittering-use-icon-storage t))))
