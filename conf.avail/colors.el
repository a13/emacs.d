(setq default-font "Consolas-10")
(set-default-font default-font)
;; daemon specific settings
(add-to-list 'default-frame-alist `(font . ,default-font))
(setq initial-frame-alist default-frame-alist)
(setq special-display-frame-alist default-frame-alist)
(setq custom-enabled-themes '(deeper-blue))
