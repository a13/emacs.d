;;; -*- lexical-binding: t; -*-
(require 'package)
(setq package-archives
      `(,@package-archives
        ("melpa" . "https://melpa.org/packages/")
        ;; ("marmalade" . "https://marmalade-repo.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ;; ("user42" . "https://download.tuxfamily.org/user42/elpa/packages/")
        ;; ("emacswiki" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/emacswiki/")
        ;; ("sunrise" . "http://joseito.republika.pl/sunrise-commander/")
        ))
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(put 'use-package 'lisp-indent-function 1)

(use-package use-package-core
  :custom
  ;; (use-package-verbose t)
  ;; (use-package-minimum-reported-time 0.005)
  (use-package-enable-imenu-support t))

(use-package gcmh
  :ensure t
  :init
  (gcmh-mode 1))

(use-package system-packages
  :ensure t
  :custom
  (system-packages-noconfirm t))

(use-package use-package-ensure-system-package :ensure t)

;; :diminish keyword
(use-package diminish :ensure t)

;; :bind keyword
(use-package bind-key :ensure t)

;; :quelpa keyword
(use-package quelpa
  :ensure t
  :defer t
  :custom
  (quelpa-update-melpa-p nil "Don't update the MELPA git repo."))

(use-package quelpa-use-package :ensure t)

(use-package use-package-custom-update
  :quelpa
  (use-package-custom-update
   :repo "a13/use-package-custom-update"
   :fetcher github
   :version original))

(use-package use-package-secrets
  :custom
  (use-package-secrets-default-directory "~/.emacs.d/secrets")
  :quelpa
  (use-package-secrets :repo "a13/use-package-secrets" :fetcher github :version original))

(use-package paradox
  :ensure t
  :defer 1
  :config
  (paradox-enable))

(use-package emacs
  :init
  (put 'narrow-to-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  :custom
  (scroll-step 1)
  (inhibit-startup-screen t "Don't show splash screen")
  (use-dialog-box nil "Disable dialog boxes")
  (x-gtk-use-system-tooltips nil)
  (enable-recursive-minibuffers t "Allow minibuffer commands in the minibuffer")
  (indent-tabs-mode nil "Spaces!")
  (debug-on-quit nil))

(use-package frame
  :bind
  ("C-z" . nil))

(use-package delsel
  :bind
  (:map mode-specific-map
        ("C-g" . minibuffer-keyboard-quit)))

(use-package simple
  :custom
  (kill-ring-max 3000)
  :diminish
  (visual-line-mode . " ↩")
  (auto-fill-function . " ↵")
  :config
  (column-number-mode t)
  (toggle-truncate-lines 1)
  :bind
  ;; remap ctrl-w/ctrl-h
  (("C-w" . backward-kill-word)
   ("C-h" . delete-backward-char)
   :map ctl-x-map
   ("C-k" . kill-region)
   ("K" . kill-current-buffer)))

(use-package help
  :bind
  (("C-?" . help-command)
   :map mode-specific-map
   ("h" . help-command)))

(use-package ibuffer
  :bind
  ([remap list-buffers] . ibuffer))

(use-package files
  :hook
  (before-save . delete-trailing-whitespace)
  :custom
  (require-final-newline t)
  ;; backup settings
  (backup-by-copying t)
  (backup-directory-alist
   `((".*" . ,(expand-file-name
               (concat user-emacs-directory "backups")))))
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t))

(use-package autorevert
  :defer 0.1
  :diminish auto-revert-mode)

(use-package recentf
  :defer 0.1
  :custom
  (recentf-auto-cleanup 30)
  :config
  (run-with-idle-timer 30 t 'recentf-save-list))

(use-package iqa
  :ensure t
  :custom
  (iqa-user-init-file (concat user-emacs-directory "README.org") "Edit README.org by default.")
  :config
  (iqa-setup-default))

(use-package cus-edit
  :custom
  (custom-file null-device "Don't store customizations"))

(use-package vlf
  :defer t
  :ensure t
  :after (ivy counsel)
  :config
  (ivy-add-actions 'counsel-find-file '(("l" vlf "view large file"))))

(use-package epa
  :defer t
  :custom
  (epg-gpg-program "gpg")
  (epa-pinentry-mode nil))

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package tramp
  :defer t
  :config
  (put 'temporary-file-directory 'standard-value `(,temporary-file-directory))
  :custom
  (tramp-backup-directory-alist backup-directory-alist)
  (tramp-default-method "ssh")
  (tramp-default-proxies-alist nil))

(use-package sudo-edit
  :ensure t
  :bind (:map ctl-x-map
              ("M-s" . sudo-edit)))

(use-package exec-path-from-shell
  :ensure t
  :defer 0.1
  :config
  (exec-path-from-shell-initialize))

(use-package em-smart
  :defer t
  :config
  (eshell-smart-initialize)
  :custom
  (eshell-where-to-jump 'begin)
  (eshell-review-quick-commands nil)
  (eshell-smart-space-goes-to-end t))

(use-package esh-help
  :ensure t
  :defer t
  :config
  (setup-esh-help-eldoc))

(use-package esh-autosuggest
  :ensure t
  :hook (eshell-mode . esh-autosuggest-mode))

(use-package eshell-prompt-extras
  :ensure t
  :after esh-opt
  :custom
  (eshell-prompt-function #'epe-theme-dakrone))

(use-package eshell-toggle
  :ensure t
  :custom
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  :bind
  ("M-`" . eshell-toggle))

(use-package eshell-fringe-status
  :ensure t
  :hook
  (eshell-mode . eshell-fringe-status-mode))

(use-package ls-lisp
  :defer t
  :custom
  (ls-lisp-emulation 'MS-Windows)
  (ls-lisp-ignore-case t)
  (ls-lisp-verbosity nil))

(use-package dired
  :custom (dired-dwim-target t "guess a target directory")
  :hook
  (dired-mode . dired-hide-details-mode))

(use-package dired-x
  :bind
  ([remap list-directory] . dired-jump)
  :custom
  ;; do not bind C-x C-j since it's used by jabber.el
  (dired-bind-jump nil))

(use-package dired-toggle
  :ensure t
  :defer t)

(use-package dired-hide-dotfiles
  :ensure t
  :bind
  (:map dired-mode-map
        ("." . dired-hide-dotfiles-mode))
  :hook
  (dired-mode . dired-hide-dotfiles-mode))

(use-package diredfl
  :ensure t
  :hook
  (dired-mode . diredfl-mode))

(use-package async
  :ensure t
  :defer t
  :init
  (dired-async-mode t))

(use-package dired-rsync
  :ensure t
  :bind
  (:map dired-mode-map
        ("r" . dired-rsync)))

(use-package dired-launch
  :ensure t
  :hook
  (dired-mode . dired-launch-mode))

(use-package dired-git-info
  :ensure t
  :bind
  (:map dired-mode-map
        (")" . dired-git-info-mode)))

(use-package mule
  :config
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-terminal-coding-system 'utf-8))

(use-package ispell
  :defer t
  :custom
  (ispell-local-dictionary-alist
   '(("russian"
      "[АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюяіїєґ’A-Za-z]"
      "[^АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюяіїєґ’A-Za-z]"
      "[-']"  nil ("-d" "uk_UA,ru_RU,en_US") nil utf-8)))
  (ispell-program-name "hunspell")
  (ispell-dictionary "russian")
  (ispell-really-aspell nil)
  (ispell-really-hunspell t)
  (ispell-encoding8-command t)
  (ispell-silently-savep t))

(use-package flyspell
  :defer t
  :custom
  (flyspell-delay 1))

(use-package flyspell-correct-ivy
  :defer t
  :ensure t
  :demand t
  :bind (:map flyspell-mode-map
              ("C-c $" . flyspell-correct-at-point)))

(use-package faces
  :defer 0.1
  :custom
  (face-font-family-alternatives '(("Consolas" "Monaco" "Monospace")))
  :config
  (set-face-attribute 'default
                      nil
                      :family (caar face-font-family-alternatives)
                      :weight 'regular
                      :width 'semi-condensed
                      ;; (/ (* 19 (display-pixel-height)) (display-mm-height))
                      :height 160)
  (set-fontset-font "fontset-default" 'cyrillic
                    (font-spec :registry "iso10646-1" :script 'cyrillic)))

(use-package lor-theme
  :config
  (load-theme 'lor t)
  :quelpa
  (lor-theme :repo "a13/lor-theme" :fetcher github :version original))

(use-package tool-bar
  :config
  (tool-bar-mode -1))

(use-package scroll-bar
  :config
  (scroll-bar-mode -1))

(use-package menu-bar
  :config
  (menu-bar-mode -1)
  :bind
  ([S-f10] . menu-bar-mode))

(use-package tooltip
  :defer t
  :custom
  (tooltip-mode -1))

(use-package time
  :defer t
  :custom
  (display-time-default-load-average nil)
  (display-time-24hr-format t)
  :config
  (display-time-mode t))

(use-package fancy-battery
  :ensure t
  :hook
  (after-init . fancy-battery-mode))

(use-package yahoo-weather
  :ensure t
  :bind (:map mode-specific-map
              ("w" . yahoo-weather-mode))
  :custom
  (yahoo-weather-guess-location-function #'yahoo-weather-ipinfo)
  (yahoo-weather-location "Kyiv, UA"))

(use-package font-lock+
  :quelpa
  (font-lock+ :repo "emacsmirror/font-lock-plus" :fetcher github))

(use-package all-the-icons
  :ensure t
  :defer t
  :config
  (setq all-the-icons-mode-icon-alist
        `(,@all-the-icons-mode-icon-alist
          (package-menu-mode all-the-icons-octicon "package" :v-adjust 0.0)
          (jabber-chat-mode all-the-icons-material "chat" :v-adjust 0.0)
          (jabber-roster-mode all-the-icons-material "contacts" :v-adjust 0.0)
          (telega-chat-mode all-the-icons-fileicon "telegram" :v-adjust 0.0
                            :face all-the-icons-blue-alt)
          (telega-root-mode all-the-icons-material "contacts" :v-adjust 0.0))))

(use-package all-the-icons-dired
  :ensure t
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ivy
  :ensure t
  :after ivy
  :custom
  (all-the-icons-ivy-buffer-commands '() "Don't use for buffers.")
  :config
  (all-the-icons-ivy-setup))

(use-package mood-line
  :ensure t
  :hook
  (after-init . mood-line-mode))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  :custom
  (initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (dashboard-items '((recents  . 5)
                     (bookmarks . 5)
                     (projects . 5)
                     ;; (agenda . 5)
                     (registers . 5))))

(use-package winner
  :config
  (winner-mode 1))

(use-package paren
  :defer t
  :config
  (show-paren-mode t))

(use-package hl-line
  :hook
  (prog-mode . hl-line-mode))

(use-package highlight-numbers
  :ensure t
  :hook
  (prog-mode . highlight-numbers-mode))

(use-package highlight-escape-sequences
  :ensure t
  :config (hes-mode))

(use-package hl-todo
  :ensure t
  :hook ((prog-mode . hl-todo-mode)
         (yaml-mode . hl-todo-mode)))

(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-identifiers
  :ensure t
  :custom
  (rainbow-identifiers-cie-l*a*b*-lightness 80)
  (rainbow-identifiers-cie-l*a*b*-saturation 50)
  (rainbow-identifiers-choose-face-function
   #'rainbow-identifiers-cie-l*a*b*-choose-face)
  :hook
  (emacs-lisp-mode . rainbow-identifiers-mode) ; actually, turns it off
  (prog-mode . rainbow-identifiers-mode))

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :hook prog-mode)

;; counsel-M-x can use this one
(use-package smex :ensure t :defer t)

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :custom
  ;; (ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (ivy-count-format "%d/%d " "Show anzu-like counter")
  (ivy-use-selectable-prompt t "Make the prompt line selectable")
  :custom-face
  (ivy-current-match ((t (:inherit 'hl-line))))
  :bind
  (:map mode-specific-map
        ("C-r" . ivy-resume))
  :config
  (ivy-mode t))

(use-package ivy-xref
  :ensure t
  :defer t
  :custom
  (xref-show-xrefs-function #'ivy-xref-show-xrefs "Use Ivy to show xrefs"))

(use-package counsel
  :ensure t
  :bind
  (([remap menu-bar-open] . counsel-tmm)
   ([remap insert-char] . counsel-unicode-char)
   ([remap isearch-forward] . counsel-grep-or-swiper)
   :map mode-specific-map
   :prefix-map counsel-prefix-map
   :prefix "c"
   ("a" . counsel-apropos)
   ("b" . counsel-bookmark)
   ("B" . counsel-bookmarked-directory)
   ("c w" . counsel-colors-web)
   ("c e" . counsel-colors-emacs)
   ("d" . counsel-dired-jump)
   ("f" . counsel-file-jump)
   ("F" . counsel-faces)
   ("g" . counsel-org-goto)
   ("h" . counsel-command-history)
   ("H" . counsel-minibuffer-history)
   ("i" . counsel-imenu)
   ("j" . counsel-find-symbol)
   ("l" . counsel-locate)
   ("L" . counsel-find-library)
   ("m" . counsel-mark-ring)
   ("o" . counsel-outline)
   ("O" . counsel-find-file-extern)
   ("p" . counsel-package)
   ("r" . counsel-recentf)
   ("s g" . counsel-grep)
   ("s r" . counsel-rg)
   ("s s" . counsel-ag)
   ("t" . counsel-org-tag)
   ("v" . counsel-set-variable)
   ("w" . counsel-wmctrl)
   :map help-map
   ("F" . counsel-describe-face))
  :init
  (counsel-mode))

(use-package swiper :ensure t)

(use-package counsel-world-clock
  :ensure t
  :after counsel
  :bind
  (:map counsel-prefix-map
        ("C" .  counsel-world-clock)))

(use-package ivy-rich
  :ensure t
  :config
  (ivy-rich-mode 1))

(use-package isearch
  :bind
  ;; TODO: maybe get a keybinding from global map
  (:map isearch-mode-map
        ("C-h" . isearch-delete-char)))

(use-package mb-depth
  :config
  (minibuffer-depth-indicate-mode 1))

(use-package avy
  :ensure t
  :config
  (avy-setup-default)
  :bind
  (("C-:" .   avy-goto-char-timer)
   ("C-." .   avy-goto-word-1)
   :map goto-map
   ("M-g" . avy-goto-line)
   :map search-map
   ("M-s" . avy-goto-word-1)))

(use-package avy-zap
  :ensure t
  :bind
  ([remap zap-to-char] . avy-zap-to-char))

(use-package ace-jump-buffer
  :ensure t
  :bind
  (:map goto-map
        ("b" . ace-jump-buffer)))

(use-package ace-window
  :ensure t
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l) "Use home row for selecting.")
  (aw-scope 'frame "Highlight only current frame.")
  :bind
  ("M-o" . ace-window))

(use-package link-hint
  :ensure t
  :bind
  (("<XF86Search>" . link-hint-open-link)
   ("S-<XF86Search>" . link-hint-copy-link)
   :map mode-specific-map
   :prefix-map link-hint-keymap
   :prefix "l"
   ("o" . link-hint-open-link)
   ("c" . link-hint-copy-link)))

(use-package ace-link
  :ensure t
  :after link-hint ; to use prefix keymap
  :bind
  (:map link-hint-keymap
        ("l" . counsel-ace-link))
  :config
  (ace-link-setup-default))

(use-package select
  :custom
  (selection-coding-system 'utf-8)
  (select-enable-clipboard t "Use the clipboard"))

(use-package expand-region
  :ensure t
  :bind
  (("C-=" . er/expand-region)
   ("C-+" . er/contract-region)
   :map mode-specific-map
   :prefix-map region-prefix-map
   :prefix "r"
   ("(" . er/mark-inside-pairs)
   (")" . er/mark-outside-pairs)
   ("'" . er/mark-inside-quotes)
   ([34] . er/mark-outside-quotes) ; it's just a quotation mark
   ("o" . er/mark-org-parent)
   ("u" . er/mark-url)
   ("b" . er/mark-org-code-block)
   ("." . er/mark-method-call)
   (">" . er/mark-next-accessor)
   ("w" . er/mark-word)
   ("d" . er/mark-defun)
   ("e" . er/mark-email)
   ("," . er/mark-symbol)
   ("<" . er/mark-symbol-with-prefix)
   (";" . er/mark-comment)
   ("s" . er/mark-sentence)
   ("S" . er/mark-text-sentence)
   ("p" . er/mark-paragraph)
   ("P" . er/mark-text-paragraph)))

(use-package elec-pair
  :config
  (electric-pair-mode))

(use-package edit-indirect
  :ensure t
  :after expand-region ; to use region-prefix-map
  :bind
  (:map region-prefix-map
        ("r" . edit-indirect-region)))

(use-package clipmon
  :ensure t
  :defer 0.1
  :config
  (clipmon-mode))

(use-package copy-as-format
  :ensure t
  :custom
  (copy-as-format-default "slack" "or Telegram")
  :bind
  (:map mode-specific-map
        :prefix-map copy-as-format-prefix-map
        :prefix "f"
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

(use-package man
  :defer t
  :custom
  (Man-notify-method 'pushy "show manpage HERE")
  :custom-face
  (Man-overstrike ((t (:inherit font-lock-type-face :bold t))))
  (Man-underline ((t (:inherit font-lock-keyword-face :underline t)))))

(use-package info-colors
  :ensure t
  :hook
  (Info-selection #'info-colors-fontify-node))

(use-package keyfreq
  :defer 0.1
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package free-keys
  :ensure t
  :defer t
  :commands free-keys)

(use-package helpful
  :ensure t
  :defer t)

(use-package jabber
  :defer t
  :secret
  (jabber-connect-all "jabber.el.gpg")
  :config
  (setq jabber-history-enabled t
        jabber-use-global-history nil
        fsm-debug nil)
  :custom
  (jabber-auto-reconnect t)
  (jabber-chat-buffer-format "*-jc-%n-*")
  (jabber-groupchat-buffer-format "*-jg-%n-*")
  (jabber-chat-foreign-prompt-format "▼ [%t] %n> ")
  (jabber-chat-local-prompt-format "▲ [%t] %n> ")
  (jabber-muc-colorize-foreign t)
  (jabber-muc-private-buffer-format "*-jmuc-priv-%g-%n-*")
  (jabber-rare-time-format "%e %b %Y %H:00")
  (jabber-resource-line-format "   %r - %s [%p]")
  (jabber-roster-buffer "*-jroster-*")
  (jabber-roster-line-format "%c %-17n")
  (jabber-roster-show-bindings nil)
  (jabber-roster-show-title nil)
  (jabber-roster-sort-functions (quote (jabber-roster-sort-by-status jabber-roster-sort-by-displayname jabber-roster-sort-by-group)))
  (jabber-show-offline-contacts nil)
  (jabber-show-resources nil))

(use-package jabber-otr
  :ensure t
  :defer t)

(use-package point-im
  :defines point-im-reply-id-add-plus
  :after jabber
  :quelpa
  (point-im :repo "a13/point-im.el" :fetcher github :version original)
  :config
  (setq point-im-reply-id-add-plus nil)
  :hook
  (jabber-chat-mode . point-im-mode))

(use-package slack
  :ensure t
  :secret
  (slack-start "work.el.gpg")
  :commands (slack-start)
  :custom
  (slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (slack-prefer-current-team t))

;; TODO: move somewhere
(use-package alert
  :ensure t
  :commands (alert)
  :custom
  (alert-default-style 'libnotify))

(use-package shr
  :defer t
  :custom
  (shr-use-fonts nil))

(use-package shr-color
  :defer t
  :custom
  (shr-color-visible-luminance-min 80 "Improve the contrast"))

(use-package eww
  :defer t
  :custom
  (eww-search-prefix "https://duckduckgo.com/html/?kd=-1&q="))

(use-package browse-url
  :bind
  ([f5] . browse-url)
  :config
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "x-www-browser")

  (defun feh-browse (url &rest ignore)
    "Browse image using feh."
    (interactive (browse-url-interactive-arg "URL: "))
    (start-process (concat "feh " url) nil "feh" url))

  (defun mpv-browse (url &rest ignore)
    "Browse video using mpv."
    (interactive (browse-url-interactive-arg "URL: "))
    (start-process (concat "mpv --loop-file=inf" url) nil "mpv" "--loop-file=inf" url))

  (defvar browse-url-images-re
    '("\\.\\(jpe?g\\|png\\)\\(:large\\|:orig\\)?\\(\\?.*\\)?$"
      "^https?://img-fotki\\.yandex\\.ru/get/"
      "^https?://pics\\.livejournal\\.com/.*/pic/"
      "^https?://l-userpic\\.livejournal\\.com/"
      "^https?://img\\.leprosorium\\.com/[0-9]+$")
    "Image URLs regular expressions list.")

  (defvar browse-url-videos-re
    '("\\.\\(gifv?\\|avi\\|AVI\\|mp[4g]\\|MP4\\|webm\\)$"
      "^https?://\\(www\\.youtube\\.com\\|youtu\\.be\\|coub\\.com\\|vimeo\\.com\\|www\\.liveleak\\.com\\)/"
      "^https?://www\\.facebook\\.com/.*/videos?/"))

  (setq browse-url-browser-function
        (append
         (mapcar (lambda (re)
                   (cons re #'eww-browse-url))
                 browse-url-images-re)
         (mapcar (lambda (re)
                   (cons re #'mpv-browse))
                 browse-url-videos-re)
         '(("." . browse-url-xdg-open)))))

(use-package webjump
  :bind
  (([S-f5] . webjump))
  :config
  (setq webjump-sites
        (append '(("debian packages" .
                   [simple-query "packages.debian.org" "http://packages.debian.org/" ""]))
                webjump-sample-sites)))

(use-package atomic-chrome
  :ensure t
  :defer t
  :custom
  (atomic-chrome-url-major-mode-alist
   '(("reddit\\.com" . markdown-mode)
     ("github\\.com" . gfm-mode)
     ("redmine" . textile-mode))
   "Major modes for URLs.")
  :config
  (atomic-chrome-start-server))

(use-package shr-tag-pre-highlight
  :ensure t
  ;;:defer t
  :after shr
  :config
  :custom-update
  (shr-external-rendering-functions
   '((pre . shr-tag-pre-highlight))))

(use-package google-this
  :defer t
  :ensure t
  :bind
  (:map mode-specific-map
        ("g" . 'google-this-mode-submap)))

(use-package multitran
  :ensure t
  :defer t)

(use-package imgbb
  :ensure t
  :defer t)

(use-package mu4e
  :defer t
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  ;; let's install it now, since mu4e packages aren't available yet
  :ensure-system-package (mu . mu4e))

(use-package smtpmail
  :defer t
  :custom
  (smtpmail-queue-mail nil "start in normal mode")
  ;;set up queue for offline email
  (smtpmail-queue-dir "~/.mail/queue/cur" "use `mu mkdir ~/.mail/queue` to set up first"))

(use-package mu4e-vars
  :defer t
  :custom
  (mu4e-view-show-images t "enable inline images")
  (mu4e-maildir (expand-file-name "~/.mail/work"))
  (mu4e-completing-read-function 'completing-read "ivy does all the work")
  (mu4e-get-mail-command "mbsync work" "sync with mbsync")
  (mu4e-change-filenames-when-moving t "rename files when moving, needed for mbsync")
  :config
  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types)))

(use-package mu4e-contrib
  :defer t
  :custom
  (mu4e-html2text-command 'mu4e-shr2text))

(use-package calendar
  :defer t
  :custom
  (calendar-week-start-day 1))

(use-package org
  :defer t
  ;; to be sure we have the latest Org version
  :ensure org-plus-contrib
  :hook
  (org-mode . variable-pitch-mode)
  (org-mode . visual-line-mode)
  :custom
  (org-src-tab-acts-natively t))

(use-package org-passwords
  :ensure org-plus-contrib
  :bind
  (:map org-mode-map
        ("C-c C-p p" . org-passwords-copy-password)
        ("C-c C-p u" . org-passwords-copy-username)
        ("C-c C-p o" . org-passwords-open-url)))

(use-package org-bullets
  :ensure t
  :custom
  ;; org-bullets-bullet-list
  ;; default: "◉ ○ ✸ ✿"
  ;; large: ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
  ;; Small: ► • ★ ▸
  (org-bullets-bullet-list '("•"))
  ;; others: ▼, ↴, ⬎, ⤷,…, and ⋱.
  ;; (org-ellipsis "⤵")
  (org-ellipsis "…")
  :hook
  (org-mode . org-bullets-mode))

(use-package htmlize
  :defer t
  :custom
  (org-html-htmlize-output-type 'css)
  (org-html-htmlize-font-prefix "org-"))

(use-package org-jira
  :defer t
  :custom
  (jiralib-url "http://jira:8080"))

(use-package synosaurus
  :defer t
  :ensure t
  :config
  (synosaurus-mode))

(use-package writegood-mode
  :defer t
  :ensure t)

(use-package ibuffer-vc
  :defer t
  :ensure t
  :config
  (define-ibuffer-column icon
    (:name "Icon" :inline t)
    (all-the-icons-ivy--icon-for-mode major-mode))
  :custom
  (ibuffer-formats
   '((mark modified read-only vc-status-mini " "
           (name 18 18 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " "
           filename-and-process)) "include vc status info")
  :hook
  (ibuffer . (lambda ()
               (ibuffer-vc-set-filter-groups-by-vc-root)
               (unless (eq ibuffer-sorting-mode 'alphabetic)
                 (ibuffer-do-sort-by-alphabetic)))))

(use-package gitconfig-mode
  :ensure t
  :defer t)

(use-package gitignore-mode
  :ensure t
  :defer t)

(use-package magit
  :ensure t
  :custom
  (magit-completing-read-function 'ivy-completing-read "Force Ivy usage.")
  :bind
  (:map mode-specific-map
        :prefix-map magit-prefix-map
        :prefix "m"
        (("a" . magit-stage-file) ; the closest analog to git add
         ("b" . magit-blame)
         ("B" . magit-branch)
         ("c" . magit-checkout)
         ("C" . magit-commit)
         ("d" . magit-diff)
         ("D" . magit-discard)
         ("f" . magit-fetch)
         ("g" . vc-git-grep)
         ("G" . magit-gitignore)
         ("i" . magit-init)
         ("l" . magit-log)
         ("m" . magit)
         ("M" . magit-merge)
         ("n" . magit-notes-edit)
         ("p" . magit-pull-branch)
         ("P" . magit-push-current)
         ("r" . magit-reset)
         ("R" . magit-rebase)
         ("s" . magit-status)
         ("S" . magit-stash)
         ("t" . magit-tag)
         ("T" . magit-tag-delete)
         ("u" . magit-unstage)
         ("U" . magit-update-index))))

(use-package magithub
  :ensure t
  :after magit
  :custom
  (magithub-clone-default-directory "~/git/")
  :bind
  (:map magit-prefix-map
        ("h b" . magithub-browse)
        ("h c" . magithub-clone)
        ("h C" . magithub-create)
        ("h f" . magithub-fork))
  :config
  (magithub-feature-autoinject t))

(use-package git-timemachine
  :ensure t
  :defer t)

(use-package browse-at-remote
  :ensure t
  :after link-hint
  :bind
  (:map link-hint-keymap
        ("r" . browse-at-remote)
        ("k" . browse-at-remote-kill)))

(use-package smerge-mode
  :defer t
  :diminish smerge-mode)

(use-package diff-hl
  :ensure t
  :hook
  ((magit-post-refresh . diff-hl-magit-post-refresh)
   (prog-mode . diff-hl-mode)
   (org-mode . diff-hl-mode)
   (dired-mode . diff-hl-dired-mode)))

(use-package smart-comment
  :ensure t
  :bind ("M-;" . smart-comment))

(use-package ag
  :defer t
  :ensure t)

(use-package projectile
  :ensure t
  :bind
  (:map mode-specific-map ("p" . projectile-command-map))
  :custom
  (projectile-project-root-files-functions
   '(projectile-root-local
     projectile-root-top-down
     projectile-root-bottom-up
     projectile-root-top-down-recurring))
  (projectile-completion-system 'ivy))

(use-package counsel-projectile
  :defer t
  :ensure t
  :after counsel projectile
  :config
  (counsel-projectile-mode))

(use-package ag
  :defer t
  :ensure-system-package (ag . silversearcher-ag)
  :custom
  (ag-highlight-search t "Highlight the current search term."))

(use-package dumb-jump
  :defer t
  :custom
  (dumb-jump-selector 'ivy)
  (dumb-jump-prefer-searcher 'ag))

(use-package company
  :ensure t
  :diminish company-mode
  :bind
  (:map company-active-map
        ("C-n" . company-select-next-or-abort)
        ("C-p" . company-select-previous-or-abort))
  :hook
  (after-init . global-company-mode))

(use-package company-quickhelp
  :ensure t
  :defer t
  :custom
  (company-quickhelp-delay 3)
  :config
  (company-quickhelp-mode 1))

(use-package company-shell
  :ensure t
  :after company
  :defer t
  :custom-update
  (company-backends '(company-shell)))

(use-package company-emoji
  :ensure t
  :after company
  :defer t
  ;; :ensure-system-package fonts-symbola
  :custom-update
  (company-backends '(company-emoji))

  :config
  (set-fontset-font t 'symbol
                    (font-spec :family
                               (if (eq system-type 'darwin)
                                   "Apple Color Emoji"
                                 "Symbola"))
                    nil 'prepend))

(use-package autoinsert
  :hook
  (find-file . auto-insert))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :custom
  (yas-prompt-functions '(yas-completing-prompt yas-ido-prompt))
  :config
  (yas-reload-all)
  :hook
  (prog-mode  . yas-minor-mode))

(use-package flycheck
  :diminish flycheck-mode
  :hook
  (prog-mode . flycheck-mode))

(use-package avy-flycheck
  :defer t
  :config
  (avy-flycheck-setup))

(use-package lisp
  :hook
  (after-save . check-parens))

(use-package highlight-defined
  :ensure t
  :custom
  (highlight-defined-face-use-itself t)
  :hook
  (emacs-lisp-mode . highlight-defined-mode))

(use-package highlight-quoted
  :ensure t
  :hook
  (emacs-lisp-mode . highlight-quoted-mode))

(use-package eros
  :ensure t
  :hook
  (emacs-lisp-mode . eros-mode))

(use-package suggest
  :ensure t
  :defer t)

(use-package ipretty
  :defer t
  :ensure t
  :config
  (ipretty-mode 1))

(use-package nameless
  :ensure t
  :hook
  (emacs-lisp-mode .  nameless-mode)
  :custom
  (nameless-global-aliases '())
  (nameless-private-prefix t))

;; bind-key can't bind to keymaps
(use-package erefactor
  :ensure t
  :defer t)

(use-package flycheck-package
  :ensure t
  :defer t
  :after flycheck
  (flycheck-package-setup))

;; (use-package dash
;;   :custom
;;   (dash-enable-fontlock t))

(use-package geiser
  :ensure t
  :defer t)

(use-package clojure-mode
  :ensure t
  :defer t
  :config
  (define-clojure-indent
    (if-let-failed? 'defun)
    (if-let-ok? 'defun)
    (when-let-failed? 'defun)
    (when-let-ok? 'defun)
    (attempt-all 'defun)
    (alet 'defun)
    (alet 'defun)
    (mlet 'defun)))

(use-package clojure-snippets
  :ensure t
  :defer t)

(use-package cider
  :ensure t
  :defer t
  :custom
  (cider-repl-display-help-banner nil)
  :config
  ;; sadly, we can't use :diminish keyword here, yet
  (diminish 'cider-mode
            '(:eval (format " 🍏%s" (cider--modeline-info)))))

(use-package kibit-helper
  :ensure t
  :defer t)

(use-package slime
  :ensure t
  :disabled
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl"
        lisp-indent-function 'common-lisp-indent-function
        slime-complete-symbol-function 'slime-fuzzy-complete-symbol
        slime-startup-animation nil)
  (slime-setup '(slime-fancy))
  (setq slime-net-coding-system 'utf-8-unix))

(use-package erlang
  :ensure t
  :defer t
  :custom
  (erlang-compile-extra-opts '(debug_info))
  :config
  (require 'erlang-start))


(use-package company-erlang
  :ensure t
  :hook
  (erlang-mode #'company-erlang-init))

(use-package lua-mode
  :ensure t
  :defer t)

(use-package conkeror-minor-mode
  :ensure t
  :disabled
  :defer t
  :hook
  (js-mode . (lambda ()
               (when (string-match "conkeror" (or (buffer-file-name) ""))
                 (conkeror-minor-mode 1)))))

(use-package json-mode
  :ensure t
  :defer t)

(use-package graphql-mode
  :ensure t
  :mode "\\.graphql\\'"
  :custom
  (graphql-url "http://localhost:8000/api/graphql/query"))

(use-package sh-script
  :mode (("zshecl" . sh-mode)
         ("\\.zsh\\'" . sh-mode))
  :custom
  ;; zsh
  (system-uses-terminfo nil))

(use-package executable
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package apt-sources-list
  :ensure t)

(use-package ssh-config-mode
  :ensure t
  :init
  (autoload 'ssh-config-mode "ssh-config-mode" t)
  :mode
  (("/\\.ssh/config\\'"     . ssh-config-mode)
   ("/sshd?_config\\'"      . ssh-config-mode)
   ("/known_hosts\\'"       . ssh-known-hosts-mode)
   ("/authorized_keys2?\\'" . ssh-authorized-keys-mode))
  :hook
  (ssh-config-mode . turn-on-font-lock))

(use-package markdown-mode
  :ensure t
  :ensure-system-package markdown
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode))
  :custom
  (markdown-command "markdown"))

(use-package jira-markup-mode
  :ensure t
  :defer t
  :after atomic-chrome
  :mode ("\\.confluence$" . jira-markup-mode)
  :custom-update
  (atomic-chrome-url-major-mode-alist
   '(("atlassian\\.net$" . jira-markup-mode))))

(use-package csv-mode
  :ensure t
  :mode
  (("\\.[Cc][Ss][Vv]\\'" . csv-mode)))

(use-package restclient
  :ensure t
  :mode
  ("\\.http\\'" . restclient-mode))

(use-package restclient-test
  :ensure t
  :hook
  (restclient-mode-hook . restclient-test-mode))

(use-package ob-restclient
  :ensure t
  :after org restclient
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))

(use-package company-restclient
  :ensure t
  :after (company restclient)
  :custom-update
  (company-backends '(company-restclient)))

(use-package net-utils
  :ensure-system-package traceroute
  :bind
  (:map mode-specific-map
        :prefix-map net-utils-prefix-map
        :prefix "n"
        ("p" . ping)
        ("i" . ifconfig)
        ("w" . iwconfig)
        ("n" . netstat)
        ("p" . ping)
        ("a" . arp)
        ("r" . route)
        ("h" . nslookup-host)
        ("d" . dig)
        ("s" . smbclient)
        ("t" . traceroute)))

(use-package docker
  :ensure t
  :bind
  (:map mode-specific-map
        ("d" . docker)))

;; not sure if these two should be here
(use-package dockerfile-mode
  :ensure t
  :defer t
  :mode "Dockerfile\\'")

(use-package docker-compose-mode
  :ensure t
  :defer t)

(use-package emamux
  :ensure t
  :defer t)

(use-package debian-el
  :ensure t
  :defer t)

(use-package reverse-im
  :ensure t
  :custom-update
  (load-path "~/.xkb/contrib")
  (reverse-im-modifiers '(super))
  (reverse-im-input-methods
   (if (require 'unipunct nil t)
       "russian-unipunct"
     "russian-computer"))
  :config
  (reverse-im-mode t))

;; Local Variables:
;; eval: (add-hook 'after-save-hook (lambda ()(org-babel-tangle)) nil t)
;; End:
