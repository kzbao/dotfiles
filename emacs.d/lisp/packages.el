;;; Package configuration
;;;
;;; Configuration is written with use-package syntax.

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Use package config
(setq use-package-always-ensure t
      use-package-always-defer nil
      use-package-verbose t
      use-package-compute-statistics t)

(use-package ace-window
  :bind ([remap other-window] . ace-window))

(use-package aggressive-indent
  :bind ("C-c a" . aggressive-indent-mode))

(use-package diminish)

(use-package doom-themes
  :config
  (setq doom-vibrant-brighter-comments t
        doom-vibrant-brighter-modeline t)
  (doom-themes-org-config)
  (load-theme 'doom-vibrant t))

(use-package easy-kill
  :bind ([remap kill-ring-save] . easy-kill))

(use-package helm
  :config
  (setq helm-split-window-inside-p t)
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-b" . helm-mini)
   ("C-x b" . helm-buffers-list)
   ("C-h a" . helm-apropos)
   ("C-x C-f" . helm-find-files)
   ("C-c f" . helm-projectile-find-file)
   ("C-c s a" . helm-projectile-ag)
   ("M-y" . helm-show-kill-ring)
   (:map helm-command-map
         ("o" . helm-occur))
   (:map isearch-mode-map
         ("<tab>" . helm-swoop-from-isearch))
   (:map helm-map
         ("<tab>" . helm-execute-persistent-action)))
  :init
  (helm-mode 1)
  (helm-adaptive-mode 1)
  (helm-descbinds-mode 1)
  (setq helm-buffer-max-length nil
        helm-ff-file-name-history-use-recentf t
        helm-ff-newfile-prompt-p nil
        helm-ff-skip-boring-files t)
  :custom
  (helm-command-prefix-key "C-c h"))

(use-package helm-swoop
  :after helm
  :config
  (setq helm-multi-swoop-edit-save t
        helm-swoop-use-line-number-face t)
  :bind
  (("C-c s b" . helm-multi-swoop-all)
   (:map helm-multi-swoop-map
         ("C-r" . helm-previous-line)
         ("C-s" . helm-next-line))))

(use-package magit
  :config
  (setq magit-push-always-verify nil
        magit-revert-buffers 'silent
        magit-no-confirm '(stage-all-changes unstage-all-changes)
        magit-commit-show-diff nil
        magit-revert-buffers 1)
  :bind
  (("C-x g" . magit-status)
   ("C-c g b" . magit-blame)
   ("C-c g l" . magit-log-buffer-file)
   ("C-c g d" . magit-log-trace-definition)))

(use-package move-text
  :bind
  (("M-<up>" . move-text-up)
   ("M-<down>" . move-text-down)))

(use-package multiple-cursors
  :bind ("C-c m" . mc/edit-lines))

(use-package neotree
  :config
  (setq neo-theme 'nerd)
  :hook (neotree-mode . (lambda () (visual-line-mode -1)))
  :bind ("C-c t" . neotree-toggle))

(use-package powerline
  :config
  (setq powerline-default-separator 'slant)
  (defface powerline-set1 '((t (:background "#1f5582" :foreground "#bbc2cf" :inherit mode-line)))
    "Blue with white text")
  (defface powerline-set2 '((t (:background "#9ca0a4" :foreground "#1c1f24" :inherit mode-line)))
    "Grey with black text")
  (defface powerline-set3 '((t (:background "#1c1f24" :foreground "#7bc275" :inherit mode-line)))
    "Black with green text")
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (face1 (if active 'powerline-set1 'powerline-inactive1))
                          (face2 (if active 'powerline-set2 'powerline-inactive2))
                          (face3 (if active 'powerline-set3 'powerline-inactive1))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (height 20)
                          (lhs (list (powerline-raw "%*" face1 'l)
                                     (powerline-buffer-id face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2 height)
                                     (when (and (boundp 'which-function-mode) which-function-mode)
                                       (powerline-raw which-func-current face2 'l))
                                     (powerline-raw " " face2)
                                     (funcall separator-left face2 face3 height)
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face3 'l))
                                     (powerline-major-mode face3 'l)))
                          (rhs (list (powerline-raw global-mode-string face3 'r)
                                     (powerline-vc face3 'r)
                                     (funcall separator-right face3 face1 height)
                                     (powerline-raw " " face1)
                                     (powerline-raw "%l" face1 'r)
                                     (powerline-raw ":" face1 'r)
                                     (powerline-raw "%c" face1 'r))))
                     (concat (powerline-render lhs)
                             (powerline-fill face3 (powerline-width rhs))
                             (powerline-render rhs)))))))

(use-package projectile
  :diminish projectile-mode
  :init (projectile-mode 1)
  :config
  (setq projectile-enable-caching t
        projectile-completion-system 'helm)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package rainbow-mode)

(use-package typescript-mode)

(use-package undo-tree
  :diminish undo-tree-mode
  :init (global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        undo-tree-auto-save-history nil))

(use-package web-mode
  :mode "\\.html?\\'"
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t
        web-mode-enable-auto-closing t
        web-mode-enable-auto-opening t
        web-mode-enable-auto-pairing t))

(use-package yaml-mode
  :mode "\\.yml\\'")

(use-package yasnippet
  :diminish yas-minor-mode
  :config (yas-global-mode 1))

(provide 'packages)
