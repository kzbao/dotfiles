;;; This is Kevin Bao's init.el.

(setq user-full-name "Kevin Bao"
      user-mail-address "kzb@kevinbao.com")

(setq lisp-dir (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path lisp-dir)

(load-library "config")
(load-library "aliases")
(load-library "functions")
(load-library "languages")
(load-library "packages")

;(find-file "~/Dropbox/Main/personal.org")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'init)
