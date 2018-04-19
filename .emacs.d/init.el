(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(load-library "options")
(load-library "aliases")
(load-library "packages")

;; Open org file
(find-file "~/Dropbox/Main/todo.org")
