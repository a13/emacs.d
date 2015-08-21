(setq default-font "Consolas-10")
(set-frame-font default-font)
;; daemon specific settings
(add-to-list 'default-frame-alist `(font . ,default-font))
(setq initial-frame-alist default-frame-alist)
(setq display-buffer-alist default-frame-alist)
(setq custom-enabled-themes '(deeper-blue))
