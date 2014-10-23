(with-feature smex
  (setq smex-save-file "~/.cache/emacs/smex-items")
  (smex-initialize)
  (global-set-key "\M-x" 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))
 
