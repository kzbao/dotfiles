;;; Language specific settings

;; CSS
(setq css-indent-level 2
      css-indent-offset 2)

;; JS
(setq js-indent-level 2)

;; LilyPond
(add-to-list 'load-path "/opt/homebrew/Cellar/lilypond/2.24.3/share/emacs/site-lisp/lilypond")
(autoload 'LilyPond-mode "lilypond-mode" "LilyPond editing mode." t)
(add-to-list 'auto-mode-alist '("\\.ly\\'" . LilyPond-mode))

(provide 'languages)
