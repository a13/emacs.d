;; remap ctrl-w
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
;; remap ctrl-h
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-x\C-b" 'ibuffer-list-buffers)

(define-key global-map [f13] 'toggle-input-method)
(define-key isearch-mode-map [f13] 'isearch-toggle-input-method)

(define-key global-map [f9] 'menu-bar-mode)
