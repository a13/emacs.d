;;; init.el stuff

;;; Code:
(setq
 conf-available (concat user-emacs-directory "conf.avail")
 conf-enabled (concat user-emacs-directory "conf.d")
 custom-file (concat conf-enabled "/99custom.el")
 lib-dir (concat user-emacs-directory "lib"))


;; (dolist (path '(lib-dir (concat base-directory "/elpa")))
;;   (when path
;;     (let ((default-directory (eval path)))
;;       (normal-top-level-add-to-load-path '("."))
;;       (normal-top-level-add-subdirs-to-load-path))))

;; (debian-run-directories conf-enabled)

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lib" user-emacs-directory)))

(add-to-list 'exec-path "~/bin/")

(setq url-request-method "GET")
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("sunrise" . "http://joseito.republika.pl/sunrise-commander/")))


;;(load-file internal-config) ;; init?
;;(load-file interface-config) ;;colors

(load-file (concat conf-enabled "/00init.el"))

(require 'package)
(package-initialize)

(defun package-install-if-not (package)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

(package-install-if-not 'use-package)

(setq package-enable-at-startup nil)

(require 'use-package)
(put 'use-package 'lisp-indent-function 1)
(setq use-package-always-ensure t)

(require 'root-edit)

(use-package smex
  :config
  (setq smex-save-file "~/.cache/emacs/smex-items")
  (smex-initialize)
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)))

(use-package anzu
  :config
  (global-anzu-mode +1)
  (set-face-attribute 'anzu-mode-line nil
                      :foreground "firebrick" :weight 'bold)
  (custom-set-variables
   '(anzu-mode-lighter "")
   '(anzu-deactivate-region t)
   '(anzu-search-threshold 1000)
   '(anzu-replace-to-string-separator " => "))
  :bind
  (("M-%" . anzu-query-replace)
   ("C-M-%" . anzu-query-replace-regexp)))

(use-package jabber
  :init
  (setq dired-bind-jump nil)
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
   '(jabber-avatar-set t)
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

(use-package w3m
  :config
  (add-hook 'w3m-mode-hook 'w3m-lnum-mode)
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

(use-package eww-lnum
  :config
  (define-key eww-mode-map "f" 'eww-lnum-follow)
  (define-key eww-mode-map "F" 'eww-lnum-universal))

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package avy
  :config
  (avy-setup-default)
  (global-set-key (kbd "C-:") 'avy-goto-char)
  ;; (global-set-key (kbd "C-'") 'avy-goto-char-2)
  (global-set-key (kbd "M-g M-g") 'avy-goto-line)
  (global-set-key (kbd "M-g w") 'avy-goto-word-1)
  ;; TODO: pull request to avy
  (defun quail-reverse-translate (char)
    "Reverse translate CHAR."
    (or
     (pcase (quail-map)
       (`(,_ ,maps)
        (dolist (map maps)
          (when (and (consp map)
                     (equal (second map)
                            (vector (char-to-string char))))
            (return (first map))))))
     char))

  (defun read-char-translated (orig-read-char &rest args)
    (let ((char (apply orig-read-char args)))
      (and char (quail-reverse-translate char))))

  (defun avy--read-char-override (orig-fun &rest args)
    (advice-add 'read-char :around #'read-char-translated)
    (apply orig-fun args)
    (advice-remove 'read-char #'read-char-translated))

  (advice-add 'avy--read-candidates :around #'avy--read-char-override))


(use-package quelpa)
(use-package quelpa-use-package)

(use-package geiser)

(use-package projectile)
(use-package clojure-mode)
(use-package cider)

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

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

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-identifiers
  :config
  (add-hook 'prog-mode-hook #'rainbow-identifiers-mode))

(use-package rainbow-mode
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
  (point-im-mode t))

(use-package reverse-im
  :ensure nil
  :quelpa
  (reverse-im :repo "a13/reverse-im.el" :fetcher github :version original)
  :config
  (add-to-list 'load-path "~/.xkb/contrib")
  (if (require 'unipunct nil t)
      (reverse-im-activate "russian-unipunct")
    (reverse-im-activate "russian-computer")))

(load-file custom-file)

(provide 'init)
;;; init.el ends
