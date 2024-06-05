;;; Functions

(defun beginning-of-line-or-indentation ()
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))

(provide 'functions)
