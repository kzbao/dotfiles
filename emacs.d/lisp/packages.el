(require 'package)
(add-to-list 'package-archives '("melpa" . "https://stable.melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-always-ensure t
      use-package-verbose t)

(use-package aggressive-indent
  :bind
  ("C-c a" . aggressive-indent-mode))

(use-package company
  :diminish company-mode
  :config
  (setq company-idle-delay 0.3)
  (setq company-minimum-prefix-length 2)
  (global-company-mode t)
  :bind
  ("C-c c" . company-mode))

(use-package helm
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

(use-package magit
  :bind
  (("C-x g" . magit-status)
  ("C-c g b" . magit-log-buffer-file)
  ("C-c g d" . magit-log-trace-definition)))

(use-package neotree
  :config
  (setq neo-theme 'nerd)
  :bind
  ("C-c t" . neotree-toggle))

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

(use-package projectile
  :config
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'helm)
  (projectile-global-mode 1))

(use-package helm-projectile
  :config
  (helm-projectile-on)
  :bind
  (("C-c C-f" . helm-projectile-find-file)
   ("C-c C-s" . helm-projectile-ag)))

(use-package rainbow-mode)

(setq custom-safe-themes t)
(use-package solarized-theme
  :config
  (setq solarized-use-variable-pitch nil)
  (setq x-underline-at-descent-line t)
  (load-theme 'solarized-dark t))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t)
  (global-undo-tree-mode 1))

(use-package web-mode
  :mode ("\\.html?\\'" . web-mode)
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-auto-closing t
        web-mode-enable-auto-opening t
        web-mode-enable-auto-pairing t))

(use-package which-key
  :config
  (which-key-mode 1)
  :bind
  ("C-x w" . which-key-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))
