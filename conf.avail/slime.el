(setq inferior-lisp-program "/usr/bin/sbcl"
      lisp-indent-function 'common-lisp-indent-function
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      slime-startup-animation nil)
(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
(require 'slime)
;(set-language-environment "utf-8")
(setq slime-net-coding-system 'utf-8-unix)
(slime-setup '(slime-fancy))
