;;; init.el --- Where it all begins
;;;
;;; Commentary:
;;;   This is Kevin Bao's init.el.
;;;
;;; Code:
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp")
(load-library "config")
(load-library "packages")
(load-library "aliases")

(setq user-full-name "Kevin Bao"
      user-mail-address "kzbao93@gmail.com")

(find-file "~/Dropbox/Main/personal.org")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'init)
;;; init.el ends here
