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

;;; internal packages setup

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
  (([f9] . menu-bar-mode)))

;;; fonts & colors

(use-package frame
  :ensure nil
  :init
  (defvar default-font "Consolas-10")
  :config
  (set-frame-font default-font)
  (add-to-list 'default-frame-alist `(font . ,default-font))
  (setq initial-frame-alist default-frame-alist)
  (set-fontset-font "fontset-default" 'cyrillic
                    (font-spec :registry "iso10646-1" :script 'cyrillic)))

(use-package window
  :ensure nil
  :config
  ;; daemon specific settings
  (setq display-buffer-alist default-frame-alist))

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
  :config
  (column-number-mode t)
  (toggle-truncate-lines 1)
  :bind
  ;; remap ctrl-w/ctrl-h
  (("C-w" . backward-kill-word)
   ("C-x C-k" . kill-region)
   ("C-h" . delete-backward-char)))

(use-package isearch
  :ensure nil
  :bind
  (:map isearch-mode-map
        ("C-h" . isearch-delete-char)))


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
  :config
  ;; zsh
  (add-to-list 'auto-mode-alist '("zshecl" . sh-mode))
  (add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
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



;;;
