;; init stuff

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

;; (defun require* (package)
;;   (unless (require package nil t)
;;     (unless package-archive-contents
;;       (package-refresh-contents))
;;     (let ((pkg-desc (assq package package-archive-contents)))
;;       (when pkg-desc
;;         (package-initialize)
;;         (package-install package)
;;         (require package)))))

(defun package-install-if-not (package)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

(package-install-if-not 'use-package)

(setq package-enable-at-startup nil)

(require 'use-package)
(put 'use-package 'lisp-indent-function 1)
(setq use-package-always-ensure t)

(require 'layout)
(require 'unipunct)
(require 'root-edit)
(remove-hook 'after-make-frame-functions #'reverse-russian-computer)
(activate-reverse-im "russian-unipunct")



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

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package ace-jump-mode
  :config
  ;; Make a simple regexp-base jump mode
  (defmacro ace-jump-make-regexp-mode (mode-name re &optional doc)
    "Create simple regexp-base AceJump mode
Example: (ace-jump-make-regexp-mode my-ace-jump-line-mode \"^\" \"AceJump line mode.\\n Marked each no empty line and move there\")"
    (let ((doc (or doc (concat "AceJump " (symbol-name mode-name)))))
      `(defun ,mode-name ()
         ,doc
         (interactive)
         (if ace-jump-current-mode (ace-jump-done))
         (setq ace-jump-current-mode mode-name)
         (ace-jump-do ,re))))
  (ace-jump-make-regexp-mode ace-jump-url-mode "\\(f\\|ht\\)tps?://")
  (define-key global-map (kbd "C-c f") 'ace-jump-url-mode))

(use-package quelpa)
(use-package quelpa-use-package)

(use-package point
  :ensure nil
  :quelpa
  (point :url "https://raw.githubusercontent.com/rayslava/emacs-point-el/master/point.el" :fetcher url :version original)
  :config
  (setq point-icon-mode nil)
  (setq point-reply-id-add-plus nil))

(load-file custom-file)
