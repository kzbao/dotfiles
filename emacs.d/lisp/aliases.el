;;; aliases.el --- Aliases

(require 'functions)

(global-set-key (kbd "C-x e") 'eshell)
(global-set-key (kbd "C-x C-e") 'eval-region)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-c k") 'kmacro-keymap)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-p") 'beginning-of-buffer)
(global-set-key (kbd "M-n") 'end-of-buffer)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key [remap move-beginning-of-line] 'beginning-of-line-or-indentation)

(provide 'aliases)
;;; aliases.el ends here
