(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq load-prefer-newer t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package ace-window
  :ensure t
  :config
  (global-set-key [remap other-window] 'ace-window))

(use-package aggressive-indent
  :ensure t
  :bind
  ("C-c a" . aggressive-indent-mode))

(use-package company
  :ensure t
  :defer 2
  :diminish company-mode
  :config
  (setq company-idle-delay 0.1
        company-show-numbers t
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t)
  (global-company-mode 1)
  :bind
  ("C-c c" . company-mode))

(use-package diminish
  :ensure t)

(setq custom-safe-themes t)

(use-package doom-themes
  :ensure t
  :config
  (setq doom-vibrant-brighter-modeline t
        doom-vibrant-brighter-comments t)
  (load-theme 'doom-vibrant t)
  (doom-themes-org-config))

(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

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
  (helm-adaptive-mode 1)
  (helm-mode 1)
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
   ("M-x" . helm-select-action)))

(use-package helm-ag
  :ensure t)

(use-package helm-descbinds
  :ensure t
  :config
  (helm-descbinds-mode 1))

(use-package helm-projectile
  :ensure t
  :bind
  (("C-c f" . helm-projectile-find-file)
   ("C-c s" . helm-projectile-ag)))

(use-package helm-swoop
  :ensure t
  :config
  (setq helm-multi-swoop-edit-save t
        helm-swoop-use-line-number-face t)
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
   :map helm-multi-swoop-map
   ("C-r" . helm-previous-line)
   ("C-s" . helm-next-line)))

(use-package key-chord
  :ensure t
  :defer 2
  :config
  (key-chord-mode 1))

(use-package magit
  :ensure t
  :bind
  (("C-x g" . magit-status)
   ("C-c g b" . magit-blame)
   ("C-c g l" . magit-log-buffer-file)
   ("C-c g d" . magit-log-trace-definition)))

(use-package move-text
  :ensure t
  :bind
  (([(meta up)] . move-text-up)
   ([(meta down)] . move-text-down)))

(use-package multiple-cursors
  :ensure t
  :bind
  ("C-c m" . mc/edit-lines))

(use-package neotree
  :ensure t
  :config
  (setq neo-theme 'nerd)
  (add-hook 'neotree-mode-hook (lambda () (visual-line-mode -1)))
  :bind
  ("C-c t" . neotree-toggle))

(use-package perspective
  :ensure t
  :config
  (persp-mode))

(use-package powerline
  :ensure t
  :config
  (setq ns-use-srgb-colorspace nil)
  (setq powerline-default-separator 'slant)
  (defface powerline-set1 '((t (:background "#1c1f24" :foreground "#51afef" :inherit mode-line)))
    "Powerline color set 1")
  (defface powerline-set2 '((t (:background "#1f5582" :foreground "#bbc2cf" :inherit mode-line)))
    "Powerline color set 2")
  (defface powerline-set3 '((t (:background "#1c1f24" :foreground "#7bc275" :inherit mode-line)))
    "Powerline color set 3")
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (face0 (if active 'powerline-set1 'powerline-inactive1))
                          (face1 (if active 'powerline-set2 'powerline-inactive2))
                          (face2 (if active 'powerline-set3 'powerline-inactive1))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (height 20)
                          (lhs (list (powerline-raw "%*" face0 'l)
                                     (powerline-buffer-id face0 'l)
                                     (powerline-raw " " face0)
                                     (funcall separator-left face0 face1 height)
                                     (when (and (boundp 'which-function-mode) which-function-mode)
                                       (powerline-raw which-func-current face1 'l))
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2 height)
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face2 'l))
                                     (powerline-major-mode face2 'l)
                                     (powerline-process face2)
                                     (powerline-minor-modes face2 'l)))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (powerline-vc face2 'r)
                                     (funcall separator-right face2 face1 height)
                                     (powerline-raw " " face1)
                                     (powerline-raw "%l" face1 'r)
                                     (powerline-raw ":" face1 'r)
                                     (powerline-raw "%c" face1 'r))))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (setq projectile-enable-caching t
        projectile-completion-system 'helm)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode 1))

(use-package rainbow-mode
  :ensure t)

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t)
  (global-undo-tree-mode 1))

(use-package web-mode
  :ensure t
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
  :ensure t
  :defer 2
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1.0)
  (which-key-mode 1)
  :bind
  ("C-x w" . which-key-mode))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))
