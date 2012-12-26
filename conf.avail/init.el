(require 'mine)

;; modes
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(ido-mode t)
(show-paren-mode t)
(global-hl-line-mode 1)
(column-number-mode t)

;; conf/misc
(setq scroll-step 1)
(setq inhibit-splash-screen t)
(setq dired-bind-jump nil) ; do not bind C-x C-j
(setq require-final-newline t)
(setq use-dialog-box nil)

(put 'narrow-to-region 'disabled nil)

;; conf/prog
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

;; conf/edit-server
(with-feature edit-server
  (edit-server-start))

;; conf/tramp
(setq tramp-default-method "ssh")
(with-feature tramp
  (add-to-list 'tramp-default-proxies-alist
               '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist
               '((regexp-quote (system-name)) nil nil)))

;; conf/web
(with-feature browse-url
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "x-www-browser")
  (global-set-key [f5] 'browse-url))

(with-feature webjump
  (global-set-key [S-f5] 'webjump)

  (setq webjump-sites
        (append '(("debian packages" .
                   [simple-query "packages.debian.org" "http://packages.debian.org/" ""]))
                webjump-sample-sites)))

;; conf/package
(with-feature package
  (setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                           ("gnu" . "http://elpa.gnu.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/"))))
;; conf/uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; conf/eshell
(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)
