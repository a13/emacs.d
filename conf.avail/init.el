;; (require 'mine)
;; modes
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(ido-mode t)
(ido-everywhere 1)
(show-paren-mode t)
(global-hl-line-mode 1)
(column-number-mode t)

;; conf/misc
(setq scroll-step 1)
(setq inhibit-splash-screen t)
(setq require-final-newline t)
(setq use-dialog-box nil)

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; conf/prog
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

;; conf/web
(require 'browse-url)
;; (setq browse-url-browser-function 'w3m-browse-url
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
                 (cons re #'feh-browse))
               browse-url-images-re)
       (mapcar (lambda (re)
                 (cons re #'mpv-browse))
               browse-url-videos-re)
       '(("." . browse-url-xdg-open))))

(global-set-key [f5] 'browse-url)

(require 'webjump)
(global-set-key [S-f5] 'webjump)
(setq webjump-sites
      (append '(("debian packages" .
                 [simple-query "packages.debian.org" "http://packages.debian.org/" ""]))
              webjump-sample-sites))

;; conf/uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; conf/eshell
(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

;; backup
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.cache/emacs/backups"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(toggle-truncate-lines 1)


;;; fonts & colors
(setq default-font "Consolas-10")
(set-frame-font default-font)
;; daemon specific settings
(add-to-list 'default-frame-alist `(font . ,default-font))
(setq initial-frame-alist default-frame-alist)
(setq display-buffer-alist default-frame-alist)
(setq custom-enabled-themes '(deeper-blue))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; language/keyboard etc
(set-language-environment "UTF-8")
(setq slime-net-coding-system 'utf-8-unix)
;; use default font for cp1251
(set-fontset-font "fontset-default" 'cyrillic
                  (font-spec :registry "iso10646-1" :script 'cyrillic))
;; emojis 😺
(set-fontset-font
 t 'symbol
 (font-spec :family "Symbola") nil 'prepend)

;;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
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
	    (ibuffer-switch-to-saved-filter-groups "default")))

;;; Zsh
(add-to-list 'auto-mode-alist '("zshecl" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(setq system-uses-terminfo nil)

;;; keys
;; remap ctrl-w
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
;; remap ctrl-h
(global-set-key "\C-h" 'delete-backward-char)
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)

(define-key global-map [f9] 'menu-bar-mode)

;;; tramp
(require 'tramp)
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
             `((regexp-quote ,(system-name)) nil nil))
