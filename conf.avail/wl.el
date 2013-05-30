(with-feature w3m
  (setq w3m-coding-system 'utf-8
        w3m-file-coding-system 'utf-8
        w3m-file-name-coding-system 'utf-8
        w3m-input-coding-system 'utf-8
        w3m-output-coding-system 'utf-8
        w3m-terminal-coding-system 'utf-8))

(with-feature wl
  (add-hook 'wl-hook (lambda () 
                       (when (not (bound-and-true-p wl-private-file))
                         (setq wl-private-file "~/.wl.gpg")
                         (load-file wl-private-file))))
  ;;(setq wl-hook nil)
  (autoload 'wl "wl" "Wanderlust" t)
  (autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
  (autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

  (setq mel-b-ccl-module nil
        base64-internal-decoding-limit 0)

  (setq
   elmo-imap4-default-authenticate-type 'nil
   elmo-imap4-default-port 1143
   elmo-imap4-default-stream-type nil
   elmo-network-session-idle-timeout 360
   mime-browse-url-function 'browse-url-firefox
   mime-edit-message-max-length 32768
   mime-edit-split-message nil
   mime-header-accept-quoted-encoded-words t
   mime-transfer-level 8
   pgg-decrypt-automatically t
   pgg-passphrase-cache-expiry 300
   ssl-certificate-verification-policy 1
   ssl-program-arguments
   '("s_client" "-quiet" "-host" host "-port" service
     "-verify" (int-to-string ssl-certificate-verification-policy)
     "-CApath" ssl-certificate-directory)
   ssl-program-name "openssl"
   wl-draft-preview-attributes-buffer-lines 7
   wl-folder-check-async t
   wl-folder-window-width 40
   wl-generate-mailer-string-function 'wl-generate-user-agent-string-1
   wl-message-buffer-prefetch-folder-type-list nil
   wl-insert-message-id nil
   wl-message-id-domain "example.com"
   wl-message-ignored-field-list '("^.*")
   wl-message-sort-field-list    wl-message-visible-field-list
   wl-message-visible-field-list '("^From:" "^To:" "^Cc:" "^Date:" "^Subject:" "^User-Agent:" "^X-Mailer:")
   wl-message-window-size '(1 . 3)
   wl-summary-line-format "%n%T%P%1@%M/%D(%W)%h:%m %t%[%17(%c %f%) %] %s"
   wl-summary-width	nil
   wl-thread-have-younger-brother-str "+"
   wl-thread-horizontal-str           "-"
   wl-thread-indent-level 4
   wl-thread-space-str                " "
   wl-thread-vertical-str             "|"
   wl-thread-youngest-child-str       "+"))

(setq 
  elmo-maildir-folder-path "~/Maildir")
