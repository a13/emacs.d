(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ;; ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ;; ("sunrise" . "http://joseito.republika.pl/sunrise-commander/")
                         ))

(package-initialize)

(setq package-enable-at-startup nil)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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

(use-package smex)

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package ivy
  :diminish ivy-mode
  :custom
  ;; (ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (ivy-count-format "%d/%d " "Show anzu-like counter.")
  (ivy-use-selectable-prompt t "Make the prompt line selectable")
  :custom-face
  (ivy-current-match ((t (:background "gray1"))))
  :bind
  (("C-c C-r" . ivy-resume))
  :config
  (ivy-mode t))

(use-package ivy-xref
  :custom
  (xref-show-xrefs-function #'ivy-xref-show-xrefs "Use Ivy to show xrefs"))

(use-package counsel
  :bind
  (([remap menu-bar-open] . counsel-tmm)
   ([remap insert-char] . counsel-unicode-char)
   ([remap isearch-forward] . counsel-grep-or-swiper))
  :config
  (counsel-mode))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

(use-package swiper)

(use-package counsel-extras
  :ensure nil
  :quelpa
  (counsel-extras :repo "a13/counsel-extras" :fetcher github :version original)
  :bind
  (("s-p" . counsel-extras-xmms2-jump)))

(use-package ivy-rich
  :custom
  (ivy-rich-switch-buffer-name-max-length 60 "Increase max length of buffer name.")
  :config
  (dolist (cmd
           '(ivy-switch-buffer
             ivy-switch-buffer-other-window
             counsel-projectile-switch-to-buffer))
    (ivy-set-display-transformer cmd #'ivy-rich-switch-buffer-transformer)))

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
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l) "Use home row for selecting.")
  (aw-scope 'frame "Highlight only current frame.")
  :bind
  (("M-o" . ace-window)))

(use-package ace-link
  :bind
  ("C-c l l" . counsel-ace-link)
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

(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (load-file jabber-secrets-file))

;; TODO: move somewhere
(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'libnotify))

(use-package atomic-chrome
  :custom
  (atomic-chrome-url-major-mode-alist
   '(("reddit\\.com" . markdown-mode)
     ("github\\.com" . gfm-mode)
     ("redmine" . textile-mode))
   "Major modes for URLs.")
  (atomic-chrome-start-server))

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
  (google-this-mode 1)
  :custom
  (google-this-keybind (kbd "C-c g")))

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

(use-package clipmon
  :config
  (clipmon-mode))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package helpful)

(use-package emamux)

(use-package copy-as-format
  :bind
  (:prefix-map copy-as-format-prefix-map
               :prefix "C-c f"
               ("f" . copy-as-format)
               ("a" . copy-as-format-asciidoc)
               ("b" . copy-as-format-bitbucket)
               ("d" . copy-as-format-disqus)
               ("g" . copy-as-format-github)
               ("l" . copy-as-format-gitlab)
               ("c" . copy-as-format-hipchat)
               ("h" . copy-as-format-html)
               ("j" . copy-as-format-jira)
               ("m" . copy-as-format-markdown)
               ("w" . copy-as-format-mediawiki)
               ("o" . copy-as-format-org-mode)
               ("p" . copy-as-format-pod)
               ("r" . copy-as-format-rst)
               ("s" . copy-as-format-slack)))

(use-package docker
  :config
  (docker-global-mode))

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package docker-compose-mode)

(use-package restclient)

(use-package ob-restclient)

(use-package company-restclient
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package ibuffer-vc
  :custom
  (ibuffer-formats
   '((mark modified read-only vc-status-mini " "
           (name 18 18 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " "
           (vc-status 10 10 :left)
           " "
           filename-and-process)) "include vc status info")
  :hook
  (ibuffer . (lambda ()
               (ibuffer-vc-set-filter-groups-by-vc-root)
               (unless (eq ibuffer-sorting-mode 'alphabetic)
                 (ibuffer-do-sort-by-alphabetic)))))

(use-package magit
  :custom
  (magit-completing-read-function 'ivy-completing-read "Force Ivy usage."))

(use-package magithub
  :after magit
  :custom
  (magithub-clone-default-directory "~/git/")
  :config
  (magithub-feature-autoinject t))

(use-package diff-hl
  :hook
  ((magit-post-refresh . diff-hl-magit-post-refresh)
   (prog-mode . diff-hl-mode)
   (org-mode . diff-hl-mode)
   (dired-mode . diff-hl-dired-mode)))

(use-package edit-indirect)

(use-package ag
  :custom
  (ag-highlight-search t "Highlight the current search term."))

(use-package projectile
  :custom
  (projectile-completion-system 'ivy)
  :config
  (projectile-mode))

(use-package dumb-jump
  :custom
  (dumb-jump-selector 'ivy)
  (dumb-jump-prefer-searcher 'ag))

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

(use-package graphql-mode
  :custom
  (graphql-url "http://localhost:8000/api/graphql/query"))

(use-package json-mode)

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
  :ensure org-plus-contrib
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

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice '(lambda ()
                                 (setq initial-buffer-choice nil)
                                 (get-buffer "*dashboard*")))
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          ;; (agenda . 5)
                          (registers . 5))))
(use-package page-break-lines
  :config
  (global-page-break-lines-mode))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-identifiers
  :hook
  (prog-mode . rainbow-identifiers-mode))

(use-package rainbow-mode
  :diminish rainbow-mode
  :hook prog-mode)

(use-package spaceline
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

(use-package fancy-battery
  :hook
  (after-init . fancy-battery-mode))

(use-package yahoo-weather
  :custom
  (yahoo-weather-location "Kyiv, UA"))

(use-package all-the-icons
  :init
  (set-frame-font "all-the-icons" t)
  :config
  (add-to-list
   'all-the-icons-mode-icon-alist
   '(package-menu-mode all-the-icons-octicon "package" :v-adjust 0.0)))

(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package spaceline-all-the-icons
  :after spaceline
  :config
  (spaceline-all-the-icons-theme)
  (spaceline-all-the-icons--setup-package-updates)
  (spaceline-all-the-icons--setup-git-ahead)
  (spaceline-all-the-icons--setup-paradox))

(use-package all-the-icons-ivy
  :custom
  (all-the-icons-ivy-buffer-commands '() "Don't use for buffers.")
  (all-the-icons-ivy-file-commands
        '(counsel-find-file
          counsel-file-jump
          counsel-recentf
          counselа-projectile-find-file
          counsel-projectile-find-dir) "Prettify more commands.")
  :config
  (all-the-icons-ivy-setup))

(use-package dired-hide-dotfiles
  :bind
  (:map dired-mode-map
        ("." . dired-hide-dotfiles-mode))
  :hook
  (dired-mode . dired-hide-dotfiles-mode))

(use-package point-im
  :ensure nil
  :defines point-im-reply-id-add-plus
  :quelpa
  (point-im :repo "a13/point-im.el" :fetcher github :version original)
  :config
  (setq point-im-reply-id-add-plus nil)
  :hook
  (jabber-chat-mode . point-im-mode))

(use-package iqa
  :custom
  (iqa-user-init-file (concat user-emacs-directory "init.org") "Edit init.org by default.")
  :config
  (iqa-setup-default))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  :ensure t)

(use-package font-lock+
  :ensure t
  :quelpa
  (font-lock+ :repo "emacsmirror/font-lock-plus" :fetcher github))

(use-package eshell-toggle
  :ensure nil
  :quelpa
  (eshell-toggle :repo "4DA/eshell-toggle" :fetcher github :version original)
  :bind
  (("M-`" . eshell-toggle)))

(use-package magit-keys
  :ensure nil
  :quelpa
  (magit-keys :repo "a13/magit-keys.el" :fetcher github :version original)
  :config
  (magit-keys-mode t))

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
