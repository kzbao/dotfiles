(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp")
(load-library "config")
(load-library "packages")
(load-library "aliases")

(setq user-full-name "Kevin Bao"
      user-mail-address "kzbao93@gmail.com")

(find-file "~/Dropbox/Main/personal.org")
