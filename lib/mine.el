(defvar absent-features nil)
(defmacro with-feature (feature &rest body)
  (declare (indent 1) (debug t))
  (if (require feature nil t)
      (cons 'progn body)
    (progn 
      (add-to-list 'absent-features `(,feature . ,load-file-name))
      (lwarn 'emacs :warning "feature %s not found" (symbol-name feature)))))

(defun install-absent ()
  "If possible, try to absent packages failed to load and reload config"
  (interactive)
  (unless package-archive-contents
       (package-refresh-contents))
  (dolist (name-path absent-features)
    (let* ((name (car name-path))
           (path (cdr name-path))
           (pkg-desc (assq name package-archive-contents)))
      (when pkg-desc
        (package-install name)
        (load-file path)
        (when (memq name features)
          (setq absent-features (remove name-path absent-features)))))))

(with-feature cl
  (defun* quote-region (&optional (left-quote "«") (right-quote "»"))
    (interactive)
    (let ((beg (region-beginning))
	  (end (region-end)))
      (goto-char beg)
      (insert left-quote)
      (goto-char (+ 1 end))
      (insert right-quote))))


;; find-file-root

(defcustom find-file-root-prefix "/sudo::"
  "Tramp root prefix to use.")


(defadvice find-file-noselect 
  (before add-root-prefix (filename &optional nowarn rawfile wildcards))
  "Add tramp prefix to filename"
  (and (bound-and-true-p root-prefix)
       (yes-or-no-p "Use root privileges? ")
       (setq filename (concat root-prefix filename))))

(ad-activate 'find-file-noselect)

(defun find-file-as-root ()
  "Find file using root privileges"
  (interactive)
  (let ((root-prefix find-file-root-prefix))
    (call-interactively (if ido-mode 'ido-find-file 'find-file))))

(defun find-current-as-root ()
  "Reopen current file as root"
  (interactive)
  (set-visited-file-name (concat find-file-root-prefix (buffer-file-name)))
  (setq buffer-read-only nil))

(global-set-key (kbd "M-s C-x C-f") 'find-file-as-root)
(global-set-key (kbd "M-s C-x C-v") 'find-current-as-root)

(provide 'mine)
