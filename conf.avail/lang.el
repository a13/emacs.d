(set-language-environment "UTF-8")
(setq slime-net-coding-system 'utf-8-unix)
(setq default-input-method "russian-computer")
;; use default font for cp1251
(set-fontset-font "fontset-default" 'cyrillic
                  (font-spec :registry "iso10646-1" :script 'cyrillic))                                                                       
