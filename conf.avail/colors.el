(setq default-font "Consolas-10")
(set-default-font default-font)

(set-background-color "#121212")
(set-foreground-color "#f6f3e8")
(set-cursor-color "gray60")

;;; misc faces
(set-face-foreground 'link "#8ac6f2")
(set-face-bold-p 'link t)
(set-face-underline-p 'link t)

(set-face-foreground 'region "#f6f3e8")
(set-face-background 'region "#444444")
(set-face-foreground 'lazy-highlight "black")
(set-face-background 'lazy-highlight "yellow")

;; "Face name to use for comments."
(set-face-foreground 'font-lock-comment-face "#99968b") ;; brightblack
(set-face-italic-p 'font-lock-comment-face t)
;; "Face name to use for comment delimiters."
(set-face-foreground font-lock-comment-delimiter-face "#99968b") ;; brightblack
(set-face-italic-p 'font-lock-comment-delimiter-face t)
;; "Face name to use for strings."
(set-face-foreground 'font-lock-string-face "#95e454") ;; brightgreen
(set-face-italic-p 'font-lock-string-face t)
;; "Face name to use for documentation."
(set-face-foreground 'font-lock-doc-face "#99968b") ;; brightblack
(set-face-italic-p 'font-lock-doc-face t)
;; "Face name to use for keywords."
(set-face-foreground 'font-lock-keyword-face "#8ac6f2") ;; brightblue
;; "Face name to use for builtins."
(set-face-foreground 'font-lock-builtin-face "#4ae6f2") ;; cyan
;; "Face name to use for function names."
(set-face-foreground 'font-lock-function-name-face "#cad6d2") ;; brightcyan
;; "Face name to use for variable names."
(set-face-foreground 'font-lock-variable-name-face "#ececec") ;; white
;; "Face name to use for type and class names."
(set-face-foreground 'font-lock-type-face "#cae682") ;; green
;; "Face name to use for constant and label names."
(set-face-foreground 'font-lock-constant-face "#fae234") ;; yellow
;; "Face name to use for things that should stand out."
(set-face-foreground 'font-lock-warning-face "red") ;; magenta
;; "Face name to use for easy to overlook negation.
;; This can be an "!" or the "n" in "ifndef"."
(set-face-foreground 'font-lock-negation-char-face "red3") ;; brightred
;; "Face name to use for preprocessor directives."
(set-face-foreground 'font-lock-preprocessor-face "#ed8778") ;; red

(set-face-foreground 'minibuffer-prompt "#8ac6f2")
(set-face-background 'modeline "gray80")
(set-face-background 'modeline-inactive "gray10")
;;(set-face-background 'header-line "gray30")

(when (fboundp show-paren-mode)
  (set-face-foreground 'show-paren-match "#f6f3e8")
  (set-face-background 'show-paren-match "#857b6f")
  (set-face-bold-p 'show-paren-match t))

;; daemon specific settings
(add-to-list 'default-frame-alist `(font . ,default-font))
(add-to-list 'default-frame-alist '(background-color . "#121212"))
(add-to-list 'default-frame-alist '(foreground-color . "#f6f3e8"))
(add-to-list 'default-frame-alist '(cursor-color . "gray60"))
(add-to-list 'default-frame-alist '(alpha . 90))
(setq initial-frame-alist default-frame-alist)
(setq special-display-frame-alist default-frame-alist)



