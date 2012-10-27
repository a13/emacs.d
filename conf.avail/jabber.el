(with-feature jabber 
  (require 'jabber-bookmarks)

  (setq jabber-history-enabled t
        jabber-use-global-history nil
        fsm-debug nil)

  ;; load jabber-account-list from encrypted file
  (defgroup jabber-local nil
    "Local settings"
    :group 'jabber)

  (defcustom jabber-secrets-file "~/.secrets.el.gpg"
    "Jabber secrets file, sets jabber-account-list variable)"
    :group 'jabber-local)

  (defadvice jabber-connect-all (before load-jabber-secrets (&optional arg))
    "Try to load account list from secrets file"
    (unless jabber-account-list
      (when (file-readable-p jabber-secrets-file)
        (load-file jabber-secrets-file))))

  (ad-activate 'jabber-connect-all)

  ;; customized 
  (custom-set-variables
   '(jabber-auto-reconnect t)
   '(jabber-avatar-set t)
   '(jabber-chat-buffer-format "*-jc-%n-*")
   '(jabber-default-status "Ура! У меня новый emacs-jabber, скачать можно с git://emacs-jabber.git.sourceforge.net/gitroot/emacs-jabber/emacs-jabber")
   '(jabber-groupchat-buffer-format "*-jg-%n-*")
   '(jabber-muc-colorize-foreign t)
   '(jabber-muc-private-buffer-format "*-jmuc-priv-%g-%n-*")
   '(jabber-rare-time-format "%e %b %Y %H:00")
   '(jabber-resource-line-format "   %r - %s [%p]")
   '(jabber-roster-buffer "*-jroster-*")
   '(jabber-roster-line-format "%c %-17n")
   '(jabber-roster-show-bindings nil)
   '(jabber-roster-show-title nil)
   '(jabber-roster-sort-functions (quote (jabber-roster-sort-by-status jabber-roster-sort-by-displayname jabber-roster-sort-by-group)))
   '(jabber-show-offline-contacts nil)
   '(jabber-show-resources nil))

  (custom-set-faces
   '(jabber-chat-prompt-foreign ((t (:foreground "#8ac6f2" :weight bold))))
   '(jabber-chat-prompt-local ((t (:foreground "#95e454" :weight bold))))
   '(jabber-chat-prompt-system ((t (:foreground "darkgreen" :weight bold))))
   '(jabber-rare-time-face ((t (:inherit erc-timestamp-face))))
   '(jabber-roster-user-away ((t (:foreground "LightSteelBlue3" :slant italic :weight normal))))
   '(jabber-roster-user-error ((t (:foreground "firebrick3" :slant italic :weight light))))
   '(jabber-roster-user-online ((t (:foreground "gray  78" :slant normal :weight bold))))
   '(jabber-roster-user-xa ((((background dark)) (:foreground "DodgerBlue3" :slant italic :weight normal))))
   '(jabber-title-large ((t (:inherit variable-pitch :weight bold :height 2.0 :width ultra-expanded))))
   '(jabber-title-medium ((t (:inherit variable-pitch :foreground "#E8E8E8" :weight bold :height 1.2 :width expanded))))
   '(jabber-title-small ((t (:inherit variable-pitch :foreground "#adc4e3" :weight bold :height 0.7 :width semi-expanded))))))
