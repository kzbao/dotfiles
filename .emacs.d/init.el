;; Package settings
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://stable.melpa.org/packages/"))
(package-initialize)
(require 'use-package)

;; Open org file
(find-file "~/Dropbox/Main/todo.org")

;; UI cleanup
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(setq visible-bell t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Program settings
(setq select-enable-clipboard t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq org-log-done t)
(global-auto-revert-mode 1)
(winner-mode 1)
(windmove-default-keybindings)
(defalias 'yes-or-no-p 'y-or-n-p)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Text editing settings
(setq-default indent-tabs-mode nil)
(setq column-number-mode t
      line-number-mode t)
(setq require-final-newline t)
(setq line-move-visual t)
(setq scroll-margin 2)
(setq search-whitespace-regexp "[-_ \t\n]+")
(delete-selection-mode 1)
(transient-mark-mode 1)
(show-paren-mode 1)
(setq show-paren-delay 0)
(global-subword-mode 1)
(which-function-mode 1)
(setq which-func-unknown "")
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Aliases
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-x C-e") 'eval-region)

(setq custom-safe-themes t)
(use-package solarized-theme
  :config
  (setq solarized-distinct-fringe-background nil)
  (setq solarized-use-variable-pitch nil)
  (setq x-underline-at-descent-line t)
  ;(setq solarized-high-contrast-mode-line t)
  (load-theme 'solarized-dark t))

(use-package powerline
  :config
  (setq ns-use-srgb-colorspace nil)
  (setq powerline-default-separator 'slant)
  (powerline-default-theme))

(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (require 'helm-config)
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  :config
  (setq helm-split-window-in-side-p t
        helm-ff-file-name-history-use-recentf t
        helm-ff-skip-boring-files t
        helm-echo-input-inheader-line t
        helm-ff-newfile-prompt-p nil)
  (helm-mode 1)
  (helm-adaptive-mode 1)
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-m" . helm-M-x)
   ("C-x b" . helm-mini)
   ("C-x C-b" . helm-mini)
   ("C-x C-f" . helm-find-files)
   ("M-y" . helm-show-kill-ring)
   ("C-c h o" . helm-occur)
   :map helm-command-map
   ("o" . helm-occur)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action)
   ("C-i" . helm-execute-persistent-action)
   ("C-z" . helm-select-action)
   ("C-w" . backward-kill-word)))

(use-package projectile
  :ensure t
  :config
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'helm)
  (projectile-global-mode 1))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on)
  (bind-keys*
   ("C-c C-f" . helm-projectile-find-file)
   ("C-c C-s" . helm-projectile-grep)))

(use-package projectile-rails
  :diminish projectile-rails-mode
  :config
  (projectile-rails-global-mode 1))

(use-package magit
  :ensure t
  :bind
  ("C-c g" . magit-status)
  ("C-x g" . magit-status))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

;; Flymake
(use-package flymake-cursor)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(global-set-key (kbd "C-c C-f") 'flymake-popup-current-error-menu)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ag color-theme-sanityinc-tomorrow company csv-mode flymake-cursor flymake-ruby helm helm-ag helm-core helm-projectile json-mode magit poker powerline projectile projectile-rails rainbow-mode solarized-theme use-package web-mode yaml-mode yasnippet))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
