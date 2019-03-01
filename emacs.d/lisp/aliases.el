;;; aliases.el --- Aliases

(global-set-key (kbd "C-x e") 'eshell)
(global-set-key (kbd "C-x C-e") 'eval-region)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-p") 'beginning-of-buffer)
(global-set-key (kbd "M-n") 'end-of-buffer)
(global-set-key (kbd "M-/") 'hippie-expand)

(defun beginning-of-line-or-indentation ()
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))
(global-set-key [remap move-beginning-of-line]
                'beginning-of-line-or-indentation)
;;; aliases.el ends here
