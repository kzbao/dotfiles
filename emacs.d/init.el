;;; This is Kevin Bao's init.el.

(setq user-full-name "Kevin Bao"
      user-mail-address "kzb@kevinbao.com")

(setq lisp-dir (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path lisp-dir)

(load-library "config")
(load-library "functions")
(load-library "keybindings")
(load-library "languages")
(load-library "packages")
(load-library "assurance")

(find-file "~/Dropbox/Main/personal.org")
(find-file "~/Dropbox/Main/assurance.org")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'init)
