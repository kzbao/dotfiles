;;; init.el --- Where it all begins
;;;
;;; Commentary:
;;;   This is Kevin Bao's init.el.
;;;
;;; Code:
(setq gc-cons-threshold (* 20 1024 1024))

(setq lisp-dir (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path lisp-dir)

(load-library "config")
(load-library "functions")
(load-library "aliases")
(load-library "packages")

(setq user-full-name "Kevin Bao"
      user-mail-address "kzbao93@gmail.com")

(find-file "~/Dropbox/Main/personal.org")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'init)
;;; init.el ends here
