(defvar absent-features nil)
(defmacro with-feature (feature &rest body)
  (declare (indent 1) (debug t))
  (if (require feature nil t)
      (cons 'progn body)
    (progn 
      (add-to-list 'absent-features `(,feature . ,load-file-name))
      (lwarn 'emacs :warning "feature %s not found" (symbol-name feature)))))

(put 'with-feature 'lisp-indent-function 1)

(defun install-absent ()
  "If possible, try to install absent packages failed to load and reload config"
  (interactive)
  (when absent-features
    (unless package-archive-contents
      (package-refresh-contents))
    (dolist (name-path absent-features)
      (pcase name-path
        (`(,name . ,path)
         (let ((pkg-desc (assq name package-archive-contents)))
           (when pkg-desc
             (package-install name)
             (and (> (length path) 0)
                  (file-readable-p path)
                  (load-file path))
             (when (memq name features)
               (setq absent-features (remove name-path absent-features))))))))))

(with-feature cl
  (defun* quote-region (&optional (left-quote "«") (right-quote "»"))
    (interactive)
    (let ((beg (region-beginning))
	  (end (region-end)))
      (goto-char beg)
      (insert left-quote)
      (goto-char (+ 1 end))
      (insert right-quote))))

(provide 'mine)
