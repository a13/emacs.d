(with-feature anzu
  (global-anzu-mode +1)
  (global-set-key (kbd "M-%") 'anzu-query-replace)
  (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

  (set-face-attribute 'anzu-mode-line nil
                      :foreground "firebrick" :weight 'bold)

  (custom-set-variables
   '(anzu-mode-lighter "")
   '(anzu-deactivate-region t)
   '(anzu-search-threshold 1000)
   '(anzu-replace-to-string-separator " => ")))
