(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ag company csv-mode flymake-cursor helm helm-ag helm-core helm-projectile helm-swoop json-mode magit poker powerline projectile rainbow-mode solarized-theme use-package web-mode yaml-mode yasnippet zenburn-theme))))

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
(recentf-mode 1)
(winner-mode 1)
(windmove-default-keybindings)
(defalias 'sh 'ansi-term)
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
(electric-pair-mode 1)
(global-hl-line-mode 1)
(global-linum-mode 1)
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

(defun beginning-of-line-or-indentation ()
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))
(global-set-key [remap move-beginning-of-line]
                'beginning-of-line-or-indentation)

;; Package settings
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://stable.melpa.org/packages/"))
(package-initialize)
(require 'use-package)

(setq custom-safe-themes t)
(use-package solarized-theme
  :config
  (setq solarized-use-variable-pitch nil)
  (setq x-underline-at-descent-line t)
  (load-theme 'solarized-dark t))

(use-package powerline
  :config
  (setq ns-use-srgb-colorspace nil)
  (setq powerline-default-separator 'slant)
  (defface powerline-green '((t (:background "#728a05" :foreground "#042028" :inherit mode-line)))
    "Solarized green for powerline")
  (defface powerline-silver '((t (:background "#708183" :foreground "#042028" :inherit mode-line)))
    "Solarized silver for powerline")
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (face-accent (if active 'powerline-green 'powerline-inactive1))
                          (face1 (if active 'powerline-silver 'powerline-inactive2))
                          (face2 (if active 'powerline-active2 'powerline-inactive1))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (height 20)
                          (lhs (list (powerline-raw "%*" face-accent 'l)
                                     (powerline-buffer-id face-accent 'l)
                                     (powerline-raw " " face-accent)
                                     (funcall separator-left face-accent face1 height)
                                     (when (and (boundp 'which-function-mode) which-function-mode)
                                       (powerline-raw which-func-current face1 'l))
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2 height)
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face2 'l))
                                     (powerline-major-mode face2 'l)
                                     (powerline-process face2)
                                     (powerline-minor-modes face2 'l)
                                     (powerline-narrow face2 'l)))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (powerline-vc face2 'r)
                                     (funcall separator-right face2 face1 height)
                                     (powerline-raw " " face1)
                                     (powerline-raw "%l" face1 'r)
                                     (powerline-raw ":" face1 'r)
                                     (powerline-raw "%c" face1 'r)
                                     (funcall separator-right face1 face2 height)
                                     (powerline-raw " " face2)
                                     (powerline-raw "%p" face2 'r)
                                     (when powerline-display-hud
                                       (powerline-hud face-accent face2)))))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))

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
   :map helm-command-map
   ("o" . helm-occur)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action)
   ("C-i" . helm-execute-persistent-action)
   ("C-z" . helm-select-action)
   ("C-w" . backward-kill-word)))

(use-package helm-swoop
  :config
  (setq helm-multi-swoop-edit-save t)
  (setq helm-swoop-use-line-number-face t)
  :bind
  (("M-i" . helm-swoop)
   ("M-I" . helm-swoop-back-to-last-point)
   ("C-c M-i" . helm-multi-swoop)
   ("C-x M-i" . helm-multi-swoop-all)
   :map isearch-mode-map
   ("M-i" . helm-swoop-from-isearch)
   :map helm-swoop-map
   ("M-i" . helm-multi-swoop-all-from-helm-swoop)
   ("C-r" . helm-previous-line)
   ("C-s" . helm-next-line)
   ("C-w" . backward-kill-word)
   :map helm-multi-swoop-map
   ("C-r" . helm-previous-line)
   ("C-s" . helm-next-line)
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
   ("C-c C-s" . helm-projectile-ag)))

(use-package magit
  :ensure t
  :bind
  ("C-c g" . magit-status)
  ("C-x g" . magit-status))

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 2))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

;; Flymake
(use-package flymake
  :diminish flymake-mode
  :config
  (require 'flymake-cursor)
  (global-set-key (kbd "C-c f") 'flymake-popup-current-error-menu))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
