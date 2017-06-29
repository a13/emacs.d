;; modes

;;; Code:

(add-to-list 'exec-path "~/bin/")

(setq scroll-step 1)

(setq inhibit-splash-screen t)
(setq use-dialog-box nil)

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

;;; interface

(use-package tool-bar
  :ensure nil
  :config
  (tool-bar-mode -1))

(use-package scroll-bar
  :ensure nil
  :config
  (scroll-bar-mode -1))

(use-package menu-bar
  :ensure nil
  :config
  (menu-bar-mode -1)
  :bind
  (([S-f10] . menu-bar-mode)))

(use-package time
  :ensure nil
  :config
  (setq display-time-default-load-average nil)
  (setq display-time-24hr-format t)
  (display-time-mode t))


;;; fonts & colors

(use-package frame
  :ensure nil
  ;; disable suspending on C-z
  :bind
  (("C-z" . nil))
  :init
  (defvar default-font "Consolas-10")
  :config
  (set-frame-font default-font)
  (add-to-list 'default-frame-alist `(font . ,default-font))
  (setq initial-frame-alist default-frame-alist)
  (setq display-buffer-alist default-frame-alist)
  (set-fontset-font "fontset-default" 'cyrillic
                    (font-spec :registry "iso10646-1" :script 'cyrillic)))

(use-package custom
  :ensure nil
  :config
  (setq custom-enabled-themes '(deeper-blue))
  (load-theme 'deeper-blue))


;;; highlighting

(use-package paren
  :ensure nil
  :config
  (show-paren-mode t))

(use-package hl-line
  :ensure nil
  :config
  (global-hl-line-mode 1))

(use-package files
  :ensure nil
  :config
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (setq require-final-newline t)
  ;; backup settings
  (setq backup-by-copying t)
  (setq backup-directory-alist
        '(("." . "~/.cache/emacs/backups")))
  (setq delete-old-versions t)
  (setq kept-new-versions 6)
  (setq kept-old-versions 2)
  (setq version-control t))



(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward))

;;; language/keyboard etc
(use-package mule
  :ensure nil
  :config
  (set-language-environment "UTF-8"))

;;; ibuffer
(use-package ibuffer
  :ensure nil
  :bind
  (("C-x C-b" . ibuffer)))

(use-package ibuf-ext
  :ensure nil
  :config
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
              (ibuffer-switch-to-saved-filter-groups "default"))))

(use-package simple
  :ensure nil
  :diminish
  ((visual-line-mode . " ↩")
   (auto-fill-function . " ↵"))
  :config
  (column-number-mode t)
  (toggle-truncate-lines 1)
  :bind
  ;; remap ctrl-w/ctrl-h
  (("C-w" . backward-kill-word)
   ("C-x C-k" . kill-region)
   ("C-h" . delete-backward-char)))

(use-package smerge-mode
  :ensure nil
  :diminish smerge-mode)

(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode)


(use-package isearch
  :ensure nil
  :bind
  (:map isearch-mode-map
        ("C-h" . isearch-delete-char)))

(use-package delsel
  :ensure nil
  :bind
  (("C-c C-g" . minibuffer-keyboard-quit)))


;;; tramp

(use-package tramp
  :ensure nil
  :config
  (setq tramp-default-method "ssh")
  ;; TODO: tramp-root-connect-list
  ;; `("\\.lpr\\." "10\\.199\\." "10\\.0\\." ,(regexp-quote (system-name)))
  (add-to-list 'tramp-default-proxies-alist
               '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist
               '("10\\.199\\." nil nil))
  (add-to-list 'tramp-default-proxies-alist
               '("10\\.0\\." nil nil))
  (add-to-list 'tramp-default-proxies-alist
               `((regexp-quote ,(system-name)) nil nil)))


(use-package epa
  :ensure nil
  :config
  (setf epa-pinentry-mode 'loopback))

(use-package calendar
  :ensure nil
  :config
  (setq calendar-week-start-day 1))

(use-package select
  :ensure nil
  :config
  (setq select-enable-clipboard t))

;; spellchecker

(use-package ispell
  :ensure nil
  :config
  (setq ispell-local-dictionary-alist
        '(("russian"
           "[АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюяіїєґ’A-Za-z]"
           "[^АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюяіїєґ’A-Za-z]"
           "[-']"  nil ("-d" "uk_UA,ru_RU,en_US") nil utf-8))
        ispell-program-name "hunspell"
        ispell-dictionary "russian"
        ispell-really-aspell nil
        ispell-really-hunspell t
        ispell-encoding8-command t
        ispell-silently-savep t))

(use-package flyspell
  :ensure nil
  :config
  (setq flyspell-delay 1))


(use-package sh-script
  :ensure nil
  :mode (("zshecl" . sh-mode)
         ("\\.zsh\\'" . sh-mode))
  :config
  ;; zsh
  (setq system-uses-terminfo nil))


;; dired and eshell

(use-package eshell
  :ensure nil)

(use-package em-smart
  :ensure nil
  :config
  (setq eshell-where-to-jump 'begin)
  (setq eshell-review-quick-commands nil)
  (setq eshell-smart-space-goes-to-end t))

(use-package ls-lisp
  :ensure nil
  :config
  (setq ls-lisp-emulation 'MS-Windows)
  (setq ls-lisp-ignore-case t)
  (setq ls-lisp-verbosity nil))

(use-package dired-x
  :ensure nil
  :config
  ;; do not bind C-x C-j since it's used by jabber.el
  (setq dired-bind-jump nil))

;; web

(use-package eww
  :ensure nil
  :config
  (setq shr-use-fonts nil)
  (setq eww-search-prefix "https://duckduckgo.com/html/?kd=-1&q="))

(use-package browse-url
  :ensure nil
  :bind
  (([f5] . browse-url))
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


(use-package cus-edit
  :ensure nil
  :config
  (setq custom-file (concat user-emacs-directory "custom.el")))

(use-package mu4e-vars
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :ensure nil
  :config
  ;;location of my maildir
  ;; enable inline images
  (setq mu4e-view-show-images t)
  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  (setq mu4e-maildir (expand-file-name "~/.mail/work"))
  ;; ivy does all the work
  (setq mu4e-completing-read-function 'completing-read)

  ;;command used to get mail
  ;; use this for testing
  (setq mu4e-get-mail-command "true")
  ;; use this to sync with mbsync
  ;;(setq mu4e-get-mail-command "mbsync gmail")

  ;;rename files when moving
  ;;NEEDED FOR MBSYNC
  (setq mu4e-change-filenames-when-moving t))

(use-package smtpmail
  :ensure nil
  :config
  ;;set up queue for offline email
  ;;use mu mkdir  ~/Maildir/queue to set up first
  (setq smtpmail-queue-mail nil  ;; start in normal mode
        smtpmail-queue-dir "~/Maildir/queue/cur"))

(use-package org
  :config
  (setq org-src-tab-acts-natively t))


;;;
