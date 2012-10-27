;; init stuff

(setq base-directory "~/.emacs.d"
      conf-available (concat base-directory "/conf.avail")
      conf-enabled (concat base-directory "/conf.d")
      custom-file (concat conf-enabled "/99custom.el")
      lib-dir (concat base-directory "/lib"))
;; TODO: enable/disable/add-library/conf

(dolist (path '(lib-dir (concat base-directory "/elpa")))
  (when path
    (let ((default-directory (eval path)))
      (normal-top-level-add-to-load-path '("."))
      (normal-top-level-add-subdirs-to-load-path))))

(debian-run-directories conf-enabled)
