;; Open org file
(find-file "~/Dropbox/Main/todo.org")

;; UI cleanup
(setq inhibit-splash-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(winner-mode t)

;; Text editing settings
(setq-default indent-tabs-mode nil)
(setq-default column-number-mode t
	      line-number-mode t)
(setq require-final-newline t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(delete-selection-mode t)
(transient-mark-mode t)
(show-paren-mode t)
(setq show-paren-delay 0)

(winner-mode t)

;; Package settings
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(load-theme 'solarized-dark t)

(yas-global-mode t)

(require 'helm-config)
(helm-mode 1)

;; Inspired by http://tuhdo.github.io/helm-intro.html
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(setq helm-split-window-in-side-p t ; open helm buffer inside current window
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-inheader-line t)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-x C-b") 'helm-mini)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; Projectile settings
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; Magit settings
(global-set-key (kbd "C-c g") 'magit-status)

;; Other keybindings
(global-set-key (kbd "C-c f") 'flymake-popup-current-error-menu)
(global-set-key (kbd "C-x C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-x C-k") 'kill-buffer)
(global-unset-key (kbd "C-x k"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company json-mode gnugo helm helm-projectile magit mines projectile rainbow-mode solarized-theme web-mode yaml-mode yasnippet))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
