;;; init.el

;;; Code:

;;; package system init
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ;; ("marmalade" . "https://marmalade-repo.org/packages/")
                         ;; ("org" . "http://orgmode.org/elpa/")
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
(use-package diminish)

;; :bind keyword
(use-package bind-key)

;; :quelpa keyword
(use-package quelpa)
(use-package quelpa-use-package)

;;; load internal packages w/settings
(load-file (concat user-emacs-directory "internal.el"))

;;; External packages

(use-package paradox
  :init
  (paradox-enable))

;; usability packages
(use-package smex
  :defines smex-save-file
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
  :bind
  (("M-x" . counsel-M-x)
   ("C-x 8 RET" . counsel-unicode-char)))

(use-package swiper)

(use-package counsel-extras
  :ensure nil
  :quelpa
  (counsel-extras :repo "a13/counsel-extras" :fetcher github :version original)
  :bind
  (("C-s" . counsel-extras-grep-or-isearch-or-swiper)
   ("s-p" . counsel-extras-xmms2-jump)))

(use-package ivy-rich
  :defines ivy-rich-abbreviate-paths ivy-rich-switch-buffer-name-max-length
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
   ("<XF86Search>" . link-hint-open-link)
   ("C-c l c" . link-hint-copy-link)
   ("S-<XF86Search>" . link-hint-copy-link)))

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
   '(jabber-show-resources nil)))

(use-package jabber-otr)

;; w3m
(use-package w3m
  :config
  (add-hook 'w3m-mode-hook 'w3m-lnum-mode)
  (setq w3m-use-tab nil)
  (setq w3m-use-title-buffer-name t)
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
  (setq w3m-cookie-accept-bad-cookies t))

(use-package shr-tag-pre-highlight
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight))

  (when (version< emacs-version "26")
    (with-eval-after-load 'eww
      (advice-add 'eww-display-html :around
                  'eww-display-html--override-shr-external-rendering-functions))))

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package projectile
  ;;  :diminish projectile-mode
  :config
  (diminish 'projectile-mode '(:eval
                               (let ((ppn (projectile-project-name)))
                                 (unless (string= ppn "-")
                                   (format " 📂%s" ppn)))))
  (projectile-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-reload-all)
  (setq yas-prompt-functions '(yas-completing-prompt yas-ido-prompt))
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package flycheck
  :diminish flycheck-mode
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
(use-package clojure-mode-extra-font-locking)
(use-package clojure-snippets)
(use-package cider
  :init
  ;; sadly, we can't use :diminish keyword here, yet
  (diminish 'cider-mode
            '(:eval (format " 🍏%s" (cider--modeline-info)))))

(use-package kibit-helper)

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

(use-package company-quickhelp
  :config
  (company-quickhelp-mode 1)
  (setq company-quickhelp-delay 3))

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

(use-package org-password-manager
  :config
  (add-hook 'org-mode-hook 'org-password-manager-key-bindings))

(use-package org-jira
  :config
  (setq jiralib-url "http://jira:8080"))

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
  :diminish rainbow-mode
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package spaceline
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

(use-package fancy-battery
  :config
  (add-hook 'after-init-hook #'fancy-battery-mode))

(use-package mu4e-alert
  :after mu4e
  :init
  (mu4e-alert-set-default-style 'notifications)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications))

(use-package mu4e-maildirs-extension
  :after mu4e
  :defines mu4e-maildirs-extension-before-insert-maildir-hook
  :init
  (mu4e-maildirs-extension)
  :config
  ;; don't draw a newline
  (setq mu4e-maildirs-extension-before-insert-maildir-hook '()))

(use-package clipmon)

(use-package point-im
  :ensure nil
  :defines point-im-reply-id-add-plus
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


;; load custom-file
;; defined in internal.el
(when (and custom-file (file-exists-p custom-file))
  (load-file custom-file))

(provide 'init)

;;; init.el ends here
