;; juick plugin
(add-to-list 'load-path "~/git/emacs-juick-el/")
(with-feature juick
  (setq juick-icon-mode t)
  (setq juick-quote-regex "\n\\(>\\(.\\|\n\\)+?.*?\\)\n\\(\\(ht\\|f\\)tps?:\\|#\\|\n\\|\\[\\)")

  (defun xmms2-status ()
    (let ((format "${playback_status}\"${artist}\"${duration}\"0\"xmms2\"${title}\"${position}\"${url}"))
      (with-temp-buffer 
        (call-process "nyxmms2" nil t nil "current" "-f" format)
        (let ((output (split-string (buffer-string) "\"" t)))
          (if (string= (car output) "Stopped")
              (make-list 7 nil)
              (cdr output))))))

  (setq tune-timer 
        (run-at-time nil 5 '(lambda ()
                             (apply 'jabber-pep-tune-send (xmms2-status)))))

  (custom-set-variables
   '(juick-id-face ((t (:foreground "light green" :weight bold))))
   '(juick-pm-face ((t (:foreground "gold"))))
   '(juick-quote-face ((t (:foreground "gray" :slant italic))))
   '(juick-tag-face ((t (:foreground "chartreuse4" :slant italic))))
   '(juick-user-name-face ((t (:foreground "DarkOliveGreen3" :slant normal :weight bold))))))
