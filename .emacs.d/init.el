(package-initialize)

(add-to-list 'load-path "~/.emacs.d/elisp/")
(load-library "options")
(load-library "aliases")
(load-library "packages")
(load-library "config")

;; Open org file
(find-file "~/Dropbox/Main/todo.org")
