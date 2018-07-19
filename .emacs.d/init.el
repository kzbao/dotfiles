(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(load-library "options")
(load-library "packages")
(load-library "aliases")

;; Open org file
(find-file "~/Dropbox/Main/todo.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yasnippet yaml-mode web-mode use-package solarized-theme rainbow-mode projectile powerline poker neotree magit helm-swoop helm-projectile helm-ag company ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro")))))
