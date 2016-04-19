(require 'emms-setup)
(emms-standard)
(emms-default-players)

;; After loaded
;(require 'emms-info-mediainfo)
;(add-to-list 'emms-info-functions 'emms-info-mediainfo)
(require 'emms-info-metaflac)
(add-to-list 'emms-info-functions 'emms-info-metaflac)

(require 'emms-player-simple)
(require 'emms-source-file)
(require 'emms-source-playlist)
(setq emms-source-file-default-directory "~/Music/")


