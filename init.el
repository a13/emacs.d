;;; init.el

;;; Code:

;; load internal packages settings
(load-file (concat user-emacs-directory "internal.el"))

;;; package system init
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ;; ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("sunrise" . "http://joseito.republika.pl/sunrise-commander/")))

(package-initialize)

;;; use-package installation
;; TODO: move to separate file

(defun package-install-if-not (package)
  "Install PACKAGE if it's not installed yet."
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

(package-install-if-not 'use-package)

(setq package-enable-at-startup nil)

(eval-when-compile
  (require 'use-package))
(put 'use-package 'lisp-indent-function 1)
(setq use-package-always-ensure t)

;; :diminish keyword
(use-package diminish
  :config
  (diminish 'auto-revert-mode))

;; :bind keyword
(use-package bind-key)

;; :quelpa keyword
(use-package quelpa)
(use-package quelpa-use-package)

;;; External packages

;; usability packages
(use-package smex
  :config
  (setq smex-save-file "~/.cache/emacs/smex-items")
  (smex-initialize))

(use-package ivy
  :diminish ivy-mode
  :config
  ;; (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (ivy-mode t)
  (setq ivy-count-format "%d/%d ")
  :bind
  (("C-c C-r" . ivy-resume)))

(use-package counsel
  :config
  :bind
  (("M-x" . counsel-M-x)))

(use-package swiper
  :config
  (defun counsel-grep-or-isearch-or-swiper ()
    "Call `swiper' for small buffers and `counsel-grep'/`isearch-forward' for large ones."
    (interactive)
    (let ((big (> (buffer-size)
                  (if (eq major-mode 'org-mode)
                      (/ counsel-grep-swiper-limit 4)
                    counsel-grep-swiper-limit)))
          (local (and (buffer-file-name)
                      (not (buffer-narrowed-p))
                      (not (ignore-errors
                             (file-remote-p (buffer-file-name))))
                      (not (string-match
                            counsel-compressed-file-regex
                            (buffer-file-name))))))
      (if big
          (if local
              (progn
                (save-buffer)
                (counsel-grep))
            (call-interactively #'isearch-forward))
        (swiper--ivy (swiper--candidates)))))

  (defun counsel-xmms2 ()
    "Jump to \"xmms2\" track."
    (interactive)
    (let ((cands
           (split-string
            (shell-command-to-string "xmms2 list") "\n" t)))
      (ivy-read "xmms2: " cands
                :action (lambda (x)
                          (string-match "^\s*\\(->\\)?\\[\\([0-9]+\\)/[0-9]+\\]\s+\\w+" x)
                          (let ((n (match-string 2 x)))
                            (call-process-shell-command
                             (format "xmms2 jump %s" n))
                            (message x)))
                :caller 'counsel-xmms2)))
  :bind
  (("C-s" . counsel-grep-or-isearch-or-swiper)
   ("s-p" . counsel-xmms2)))

(use-package ivy-rich
  :config
  (setq ivy-rich-abbreviate-paths t)
  (setq ivy-rich-switch-buffer-name-max-length 45)
  (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer))

;; avy-based stuff
(use-package avy
  :config
  (avy-setup-default)
  :bind
  (("C-:" . avy-goto-char)
   ;; ("C-'" . avy-goto-char-2)
   ("M-g M-g" . avy-goto-line)
   ("M-g w" . avy-goto-word-1)))

(use-package ace-jump-buffer
  :bind
  (("M-g b" . ace-jump-buffer)))

(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind
  (("M-o" . ace-window)))

(use-package ace-link
  :config
  (ace-link-setup-default))

(use-package link-hint
  :ensure t
  :bind
  (("C-c l o" . link-hint-open-link)
   ("C-c l c" . link-hint-copy-link)))

;; jabber
(use-package jabber
  :config
  (setq jabber-history-enabled t
        jabber-use-global-history nil
        fsm-debug nil)
    ;; load jabber-account-list from encrypted file
  (defgroup jabber-local nil
    "Local settings"
    :group 'jabber)

  (defcustom jabber-secrets-file "~/.secrets.el.gpg"
    "Jabber secrets file, sets jabber-account-list variable)"
    :group 'jabber-local)

  (defadvice jabber-connect-all (before load-jabber-secrets (&optional arg))
    "Try to load account list from secrets file"
    (unless jabber-account-list
      (when (file-readable-p jabber-secrets-file)
        (load-file jabber-secrets-file))))

  (ad-activate 'jabber-connect-all)

  ;; customized
  (custom-set-variables
   '(jabber-auto-reconnect t)
   '(jabber-chat-buffer-format "*-jc-%n-*")
   '(jabber-default-status "")
   '(jabber-groupchat-buffer-format "*-jg-%n-*")
   '(jabber-chat-foreign-prompt-format "▼ [%t] %n> ")
   '(jabber-chat-local-prompt-format "▲ [%t] %n> ")
   '(jabber-muc-colorize-foreign t)
   '(jabber-muc-private-buffer-format "*-jmuc-priv-%g-%n-*")
   '(jabber-rare-time-format "%e %b %Y %H:00")
   '(jabber-resource-line-format "   %r - %s [%p]")
   '(jabber-roster-buffer "*-jroster-*")
   '(jabber-roster-line-format "%c %-17n")
   '(jabber-roster-show-bindings nil)
   '(jabber-roster-show-title nil)
   '(jabber-roster-sort-functions (quote (jabber-roster-sort-by-status jabber-roster-sort-by-displayname jabber-roster-sort-by-group)))
   '(jabber-show-offline-contacts nil)
   '(jabber-show-resources nil))

  (custom-set-faces
   '(jabber-chat-prompt-foreign ((t (:foreground "#8ac6f2" :weight bold))))
   '(jabber-chat-prompt-local ((t (:foreground "#95e454" :weight bold))))
   '(jabber-chat-prompt-system ((t (:foreground "darkgreen" :weight bold))))
   '(jabber-rare-time-face ((t (:inherit erc-timestamp-face))))
   '(jabber-roster-user-away ((t (:foreground "LightSteelBlue3" :slant italic :weight normal))))
   '(jabber-roster-user-error ((t (:foreground "firebrick3" :slant italic :weight light))))
   '(jabber-roster-user-online ((t (:foreground "gray  78" :slant normal :weight bold))))
   '(jabber-roster-user-xa ((((background dark)) (:foreground "DodgerBlue3" :slant italic :weight normal))))
   '(jabber-title-large ((t (:inherit variable-pitch :weight bold :height 2.0 :width ultra-expanded))))
   '(jabber-title-medium ((t (:inherit variable-pitch :foreground "#E8E8E8" :weight bold :height 1.2 :width expanded))))
   '(jabber-title-small ((t (:inherit variable-pitch :foreground "#adc4e3" :weight bold :height 0.7 :width semi-expanded))))))

(use-package jabber-otr)

;; w3m
(use-package w3m
  :config
  (add-hook 'w3m-mode-hook 'w3m-lnum-mode)
  (setq w3m-use-cookies t)
  (setq w3m-use-tab nil)
  (setq w3m-use-title-buffer-name t)
  (setq w3m-use-filter t)
  (setq w3m-enable-google-feeling-lucky t)
  (setq w3m-use-header-line-title t)
  (defun set-external-browser (orig-fun &rest args)
    (let ((browse-url-browser-function
           (if (eq browse-url-browser-function 'w3m-browse-url)
               'browse-url-generic
             browse-url-browser-function)))
      (apply orig-fun args)))
  (advice-add 'w3m-view-url-with-browse-url :around #'set-external-browser))

(use-package w3m-cookie
  :ensure nil
  :config
  (setq w3m-cookie-accept-bad-cookies 'ask)
  (setq w3m-cookie-accept-domains '("ofm" "m.last.fm" ".last.fm")))

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-reload-all)
  (setq yas-prompt-functions '(yas-completing-prompt yas-ido-prompt))
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package flycheck
  :diminish "🗸"
  :config
  (add-hook 'prog-mode-hook #'flycheck-mode))

(use-package avy-flycheck
  :config
  (avy-flycheck-setup))

(use-package nameless
  :config
  (add-hook 'emacs-lisp-mode-hook #'nameless-mode)
  (setq nameless-private-prefix t))

;; scheme
(use-package geiser)

;; clojure
(use-package clojure-mode)
(use-package clojure-snippets)
(use-package cider)

;; CL
(use-package slime
  :disabled
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl"
        lisp-indent-function 'common-lisp-indent-function
        slime-complete-symbol-function 'slime-fuzzy-complete-symbol
        slime-startup-animation nil)
  (slime-setup '(slime-fancy))
  (setq slime-net-coding-system 'utf-8-unix))

;; scala
(use-package ensime
  :bind (:map ensime-mode-map
              ("C-x C-e" . ensime-inf-eval-region)))

;; lua
(use-package lua-mode)

;; company-based plugins
(use-package company
  :diminish (company-mode . "𝍎")
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-shell
  :config
  (add-to-list 'company-backends 'company-shell))

(use-package company-emoji
  :config
  (add-to-list 'company-backends 'company-emoji)
  (set-fontset-font t 'symbol
                    (font-spec :family
                               (if (eq system-type 'darwin)
                                   "Apple Color Emoji"
                                 "Symbola"))
                               nil 'prepend))

(use-package ibuffer-vc
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package magit)

(use-package diff-hl
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'prog-mode-hook #'diff-hl-mode)
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode))

(use-package edit-indirect)

(use-package conkeror-minor-mode
  :config
  (add-hook 'js-mode-hook (lambda ()
                          (when (string-match "conkeror" (buffer-file-name))
                            (conkeror-minor-mode 1)))))

;; interface

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-identifiers
  :config
  (add-hook 'prog-mode-hook #'rainbow-identifiers-mode))

(use-package rainbow-mode
  :diminish (rainbow-mode . "🌈")
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package spaceline
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme))

(use-package point-im
  :ensure nil
  :quelpa
  (point-im :repo "a13/point-im.el" :fetcher github :version original)
  :config
  (setq point-im-reply-id-add-plus nil)
  (add-hook 'jabber-chat-mode-hook #'point-im-mode))

;; TODO
(use-package root-edit
  :disabled
  :ensure nil
  :quelpa
  (root-edit :repo "a13/root-edit.el" :fetcher github :version original)
  :bind
  ("M-s C-x C-f" . find-file-as-root)
  ("M-s C-x C-v" . find-current-as-root))

(use-package eshell-toggle
  :ensure nil
  :quelpa
  (eshell-toggle :repo "4DA/eshell-toggle" :fetcher github :version original)
  :bind
  (("M-`" . eshell-toggle)))

(use-package reverse-im
  :config
  (add-to-list 'load-path "~/.xkb/contrib")
  (reverse-im-activate
   (if (require 'unipunct nil t)
       "russian-unipunct"
     "russian-computer")))


;; defined in internal.el
(when custom-file
  (load-file custom-file))

(provide 'init)

;;; init.el ends here
