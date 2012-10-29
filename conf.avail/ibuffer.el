(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("work" (filename . "/perforce/"))
	       ("c/c++" (or (mode . c++-mode)
			    (mode . c-mode)))
	       ("jabber" (or (mode . jabber-chat-mode)
			     (mode . jabber-roster-mode)))
               ("mail" (or
                        (name . "Summary")
                        (name . "Folder")))
	       ("dired" (mode . dired-mode))
	       ("emacs" (or
			 (name . "^\\*scratch\\*$")
			 (name . "^  ")
			 (name . "^\\*Messages\\*$")
                         (name . "^\\*Warnings\\*$")
                         (mode . emacs-lisp-mode)))))))
(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))
