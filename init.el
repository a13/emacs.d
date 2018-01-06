(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ;; ("marmalade" . "https://marmalade-repo.org/packages/")
                         ;; ("org" . "http://orgmode.org/elpa/")
                         ;; ("sunrise" . "http://joseito.republika.pl/sunrise-commander/")
                         ))

(package-initialize)

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

(load-file (concat user-emacs-directory "internal.el"))

(use-package paradox
  :config
  (paradox-enable))

(use-package smex
  :defines smex-save-file
  :config
  (setq smex-save-file "~/.cache/emacs/smex-items")
  (smex-initialize))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package ivy
  :diminish ivy-mode
  :config
  ;; (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (ivy-mode t)
  (setq ivy-count-format "%d/%d ")
  :bind
  (("C-c C-r" . ivy-resume)))

(use-package counsel
  :init
  (require 'iso-transl)
  :bind
  (("M-x" . counsel-M-x)
   ("M-y" . counsel-yank-pop)
   ("C-x C-f" . counsel-find-file)
   ("C-c C-SPC" . counsel-mark-ring)
   ("<f10>" . counsel-tmm)
   :map iso-transl-ctl-x-8-map
   ("RET" . counsel-unicode-char)
   :map help-map
   ("f" . counsel-describe-function)
   ("v" . counsel-describe-variable)
   ("b" . counsel-descbinds)))

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
  (setq aw-scope 'frame)
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

(use-package eww-lnum
  :bind (:map eww-mode-map
              ("f" . eww-lnum-follow)
              ("F" . eww-lnum-universal)))


(use-package shr-tag-pre-highlight
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight))

  (when (version< emacs-version "26")
    (with-eval-after-load 'eww
      (advice-add 'eww-display-html :around
                  'eww-display-html--override-shr-external-rendering-functions))))

(use-package google-this
  :diminish google-this-mode
  :config
  (google-this-mode 1))

(use-package mu4e-alert
  :after mu4e
  :init
  (mu4e-alert-set-default-style 'notifications)
  :hook ((after-init . mu4e-alert-enable-mode-line-display)
         (after-init . mu4e-alert-enable-notifications)))

(use-package mu4e-maildirs-extension
  :after mu4e
  :defines mu4e-maildirs-extension-before-insert-maildir-hook
  :init
  (mu4e-maildirs-extension)
  :config
  ;; don't draw a newline
  (setq mu4e-maildirs-extension-before-insert-maildir-hook '()))

(use-package multitran)

(use-package sudo-edit)

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode))

(use-package helpful)

(use-package emamux)

(use-package restclient)

(use-package ob-restclient)

(use-package company-restclient
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package ibuffer-vc
  :hook
  (ibuffer . (lambda ()
               (ibuffer-vc-set-filter-groups-by-vc-root)
               (unless (eq ibuffer-sorting-mode 'alphabetic)
                 (ibuffer-do-sort-by-alphabetic)))))

(use-package magit)

(use-package diff-hl
  :hook
  ((magit-post-refresh . diff-hl-magit-post-refresh)
   (prog-mode . diff-hl-mode)
   (dired-mode . diff-hl-dired-mode)))

(use-package edit-indirect)

(use-package ag)

(use-package projectile
  :init
  (setq projectile-completion-system 'ivy)
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
  :hook
  (prog-mode  . yas-minor-mode))

(use-package flycheck
  :diminish flycheck-mode
  :hook
  (prog-mode . flycheck-mode))

(use-package avy-flycheck
  :config
  (avy-flycheck-setup))

(use-package nameless
  :hook
  (emacs-lisp-mode .  nameless-mode)
  :config
  (setq nameless-private-prefix t))

(use-package suggest)

(use-package ipretty
  :config
  (ipretty-mode 1))

(use-package geiser)

(use-package clojure-mode)
(use-package clojure-mode-extra-font-locking)
(use-package clojure-snippets)
(use-package cider
  :config
  ;; sadly, we can't use :diminish keyword here, yet
  (diminish 'cider-mode
            '(:eval (format " 🍏%s" (cider--modeline-info)))))

(use-package kibit-helper)

(use-package slime
  :disabled
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl"
        lisp-indent-function 'common-lisp-indent-function
        slime-complete-symbol-function 'slime-fuzzy-complete-symbol
        slime-startup-animation nil)
  (slime-setup '(slime-fancy))
  (setq slime-net-coding-system 'utf-8-unix))

(use-package scala-mode)

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package ensime
  :bind (:map ensime-mode-map
              ("C-x C-e" . ensime-inf-eval-region)))

(use-package lua-mode)

(use-package conkeror-minor-mode
  :hook
  (js-mode . (lambda ()
               (when (string-match "conkeror" (or (buffer-file-name) ""))
                 (conkeror-minor-mode 1)))))

(use-package company
  :diminish company-mode
  :hook
  (after-init . global-company-mode))

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

(use-package org
  :ensure t
  :pin melpa
  :init
  (setq org-src-tab-acts-natively t))

(use-package org-bullets
  :init
  ;; org-bullets-bullet-list
  ;; default: "◉ ○ ✸ ✿"
  ;; large: ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
  ;; Small: ► • ★ ▸
  (setq org-bullets-bullet-list '("•"))
  ;; others: ▼, ↴, ⬎, ⤷,…, and ⋱.
  ;; (setq org-ellipsis "⤵")
  (setq org-ellipsis "…")
  :hook
  (org-mode . org-bullets-mode))

(use-package htmlize
  :config
  (setq org-html-htmlize-output-type 'css)
  (setq org-html-htmlize-font-prefix "org-"))

(use-package org-password-manager
  :hook
  (org-mode . org-password-manager-key-bindings))

(use-package org-jira
  :config
  (setq jiralib-url "http://jira:8080"))

(use-package rainbow-delimiters
  :hook prog-mode)

(use-package rainbow-identifiers
  :hook prog-mode)

(use-package rainbow-mode
  :diminish rainbow-mode
  :hook prog-mode)

(use-package spaceline
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

(use-package fancy-battery
  :hook after-init)

(use-package clipmon
  :config
  (clipmon-mode))

(use-package point-im
  :ensure nil
  :defines point-im-reply-id-add-plus
  :quelpa
  (point-im :repo "a13/point-im.el" :fetcher github :version original)
  :config
  (setq point-im-reply-id-add-plus nil)
  :hook
  (jabber-chat-mode-hook . point-im-mode))

(use-package iqa
  :ensure t
  :init
  (setq iqa-user-init-file (concat user-emacs-directory "init.org"))
  :config
  (iqa-setup-default))

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
  (add-to-list 'reverse-im-modifiers 'super)
  (add-to-list 'reverse-im-input-methods
               (if (require 'unipunct nil t)
                   "russian-unipunct"
                 "russian-computer"))
  (reverse-im-mode t))

;; defined in internal.el
(when (and custom-file (file-exists-p custom-file))
  (load-file custom-file))

;; Local Variables:
;; eval: (add-hook 'after-save-hook (lambda ()(org-babel-tangle)) nil t)
;; End:
