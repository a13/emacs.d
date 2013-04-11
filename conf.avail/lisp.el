(with-feature paredit
  (autoload 'paredit-mode "paredit"
    "Minor mode for pseudo-structurally editing Lisp code." t)
  (let ((lisp-modes-hooks '(emacs-lisp-mode-hook lisp-mode-hook lisp-interaction-mode-hook scheme-mode-hook))
        (lisp-hooks (lambda () (progn
                            (paredit-mode +1)
                            (pretty-symbols-mode)))))
    (mapcar (lambda (mh) (add-hook mh lisp-hooks)) lisp-modes-hooks)))
