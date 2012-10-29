(c-add-style "sc"
             '("K&R"
               (c-basic-offset . 8)
               (c-offsets-alist
                (case-label . +)
                (arglist-close . c-lineup-arglist-intro-after-paren)
                (inextern-lang . 0))))

(defun maybe-sc-style ()
  "Set sc style"
  (when (or (string-match "/perforce/" buffer-file-name)
            (c-set-style "sc")
            (setq show-trailing-whitespace t
                  indent-tabs-mode t
                  tab-width 8
                  c-indent-tabs-mode t))))


(add-hook 'c-mode-common-hook 'maybe-sc-style)
(add-hook 'c++-mode-common-hook 'maybe-sc-style)

(add-to-list 'file-coding-system-alist '("/perforce/" . koi8-r-unix))
