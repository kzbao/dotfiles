;;; Key bindings

(require 'functions)

(let ((keybindings '(("C-x e" . eshell)
                     ("C-x C-e" . eval-region)
                     ("C-x C-k" . kill-this-buffer)
                     ("C-c k" . kmacro-keymap)
                     ("C-c l" . package-list-packages)
                     ("C-;" . comment-or-uncomment-region)
                     ("M-g" . goto-line)
                     ("M-p" . beginning-of-buffer)
                     ("M-n" . end-of-buffer)
                     ("M-/" . hippie-expand))))
  (dolist (kb keybindings)
    (global-set-key (kbd (car kb)) (cdr kb))))

(global-set-key [remap move-beginning-of-line] 'beginning-of-line-or-indentation)

(provide 'keybindings)
