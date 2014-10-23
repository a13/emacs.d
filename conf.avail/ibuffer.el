(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("work" (or (filename . "/perforce/")
                           (filename . "/export/git")))
	       ("dired" (mode . dired-mode))
               ("scheme" (or (mode . scheme-mode)
                             (mode . geiser-repl-mode)))

               ("search" (or (mode . grep-mode)
                             (mode . occur-mode)))
	       ("c/c++" (or (mode . c++-mode)
			    (mode . c-mode)))
               ("mail" (or
                        (name . "Summary")
                        (name . "Folder")))
	       ("jabber" (or (mode . jabber-chat-mode)
			     (mode . jabber-roster-mode)))
               ("dotfiles" (filename . "/git/dotfiles"))
               ("w3m" (mode . w3m-mode))
	       ("emacs" (or
			 (name . "^\\*scratch\\*$")
			 (name . "^  ")
			 (name . "^\\*Messages\\*$")
                         (name . "^\\*Warnings\\*$")
                         (mode . emacs-lisp-mode)))))))
(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))
