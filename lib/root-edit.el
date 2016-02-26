(defcustom find-file-root-prefix "/sudo::"
  "Tramp root prefix to use.")

(defadvice find-file-noselect 
  (before add-root-prefix (filename &optional nowarn rawfile wildcards))
  "Add tramp prefix to filename"
  (and (bound-and-true-p root-prefix)
       (yes-or-no-p "Use root privileges? ")
       (unless (file-writable-p filename)
         (setq filename (concat root-prefix filename)))))

(ad-activate 'find-file-noselect)

(defun find-file-as-root ()
  "Find file using root privileges"
  (interactive)
  (let ((root-prefix find-file-root-prefix))
    (call-interactively (if ido-mode 'ido-find-file 'find-file))))

(defun find-current-as-root ()
  "Reopen current file as root"
  (interactive)
  (let ((filename (buffer-file-name)))
    (unless (file-writable-p filename)
      (set-visited-file-name (concat find-file-root-prefix filename))
      (setq buffer-read-only nil))))

(global-set-key (kbd "M-s C-x C-f") 'find-file-as-root)
(global-set-key (kbd "M-s C-x C-v") 'find-current-as-root)

(provide 'root-edit)
