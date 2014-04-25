;; (define-key global-map [f13] 'toggle-input-method)
;; (define-key isearch-mode-map [f13] 'isearch-toggle-input-method)

(defun reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
        (modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (quail-get-translation
                      (cadr map) (char-to-string to) 1)))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key local-function-key-map
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))

(defmacro activate-reverse-im (im)
  (if (not (daemonp))
      (reverse-input-method im)
    (let ((hname (intern (format "reverse-%s" im))))
      `(progn
         (defun ,hname (f)
           (lexical-let ((frame f))
             (run-at-time nil nil
                          #'(lambda () (unless (and (daemonp) (eq frame terminal-frame))
                                         (reverse-input-method ,im))))))
         ;;                                       (remove-hook 'after-make-frame-functions #'rev-inp-m-init))))))
         (add-hook 'after-make-frame-functions #',hname)))))

(activate-reverse-im "russian-computer")

(defadvice read-passwd (around my-read-passwd act)
  (let ((local-function-key-map nil)
        (read-passwd-map (let ((map read-passwd-map))
                           (set-keymap-parent map minibuffer-local-map)
                           (define-key map [return] #'exit-minibuffer)
                           (define-key map [backspace] #'delete-backward-char)
                           map)))
    ad-do-it))

