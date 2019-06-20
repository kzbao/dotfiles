;;; packages.el --- Package configuration
;;;
;;; Commentary:
;;;   Packages are from bleeding edge MELPA.
;;;   Configuration is written with use-package syntax.
;;;
;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq load-prefer-newer t)
(setq use-package-always-ensure t)
(setq use-package-verbose t)

(use-package ace-window
  :config
  (global-set-key [remap other-window] 'ace-window))

(use-package aggressive-indent
  :bind
  ("C-c a" . aggressive-indent-mode))

(use-package company
  :diminish company-mode
  :init
  (global-company-mode)
  :config
  (setq company-echo-delay 0)
  (setq company-idle-delay 0)
  (setq company-require-match nil)
  (setq company-selection-wrap-around t)
  (setq company-show-numbers t)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
  (setq company-transformers '(company-sort-by-occurrence))
  :bind
  (("C-c c" . company-mode)
   :map company-active-map
   ("RET" . nil)
   ("<return>" . nil)
   ("<tab>" . company-complete-common-or-cycle)
   ("C-SPC" . company-complete-selection)))

(use-package diminish)

(use-package doom-themes
  :config
  (setq doom-vibrant-brighter-comments t)
  (setq doom-vibrant-brighter-modeline t)
  (load-theme 'doom-vibrant t)
  (doom-themes-org-config))

(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package helm
  :demand t
  :diminish helm-mode
  :init
  (require 'helm-config)
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  :config
  (setq helm-split-window-in-side-p t)
  (setq helm-ff-file-name-history-use-recentf t)
  (setq helm-ff-skip-boring-files t)
  (setq helm-echo-input-inheader-line t)
  (setq helm-ff-newfile-prompt-p nil)
  (helm-adaptive-mode 1)
  (helm-mode 1)
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-m" . helm-M-x)
   ("C-x b" . helm-buffers-list)
   ("C-x C-b" . helm-mini)
   ("C-x C-f" . helm-find-files)
   ("M-y" . helm-show-kill-ring)
   :map helm-command-map
   ("o" . helm-occur)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action)
   ("M-x" . helm-select-action)))

(use-package helm-ag
  :after helm)

(use-package helm-descbinds
  :after helm
  :config
  (helm-descbinds-mode 1))

(use-package helm-projectile
  :after helm
  :bind
  (("C-c f" . helm-projectile-find-file)
   ("C-c s a" . helm-projectile-ag)))

(use-package helm-swoop
  :after helm
  :config
  (setq helm-multi-swoop-edit-save t)
  (setq helm-swoop-use-line-number-face t)
  :bind
  (("C-c s s" . helm-swoop)
   ("C-c s S" . helm-multi-swoop-all)
   ("C-c s C-s" . helm-multi-swoop)
   :map isearch-mode-map
   ("<tab>" . helm-swoop-from-isearch)
   :map helm-swoop-map
   ("C-r" . helm-previous-line)
   ("C-s" . helm-next-line)
   ("C-w" . helm-yank-text-at-point)
   ("C-x" . helm-multi-swoop-all-from-helm-swoop)
   :map helm-multi-swoop-map
   ("C-r" . helm-previous-line)
   ("C-s" . helm-next-line)))

(use-package key-chord
  :init
  (key-chord-mode 1))

(use-package magit
  :config
  (set-default 'magit-push-always-verify nil)
  (set-default 'magit-revert-buffers 'silent)
  (set-default 'magit-no-confirm '(stage-all-changes unstage-all-changes))
  :bind
  (("C-x g" . magit-status)
   ("C-c g b" . magit-blame)
   ("C-c g l" . magit-log-buffer-file)
   ("C-c g d" . magit-log-trace-definition)))

(use-package move-text
  :bind
  (([(meta up)] . move-text-up)
   ([(meta down)] . move-text-down)))

(use-package multiple-cursors
  :bind
  ("C-c m" . mc/edit-lines))

(use-package neotree
  :config
  (setq neo-theme 'nerd)
  (add-hook 'neotree-mode-hook (lambda () (visual-line-mode -1)))
  :bind
  ("C-c t" . neotree-toggle))

(use-package perspective
  :config
  (persp-mode))

(use-package powerline
  :config
  (setq ns-use-srgb-colorspace nil)
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
                                     (powerline-major-mode face3 'l)
                                     (powerline-process face3)
                                     (powerline-minor-modes face3 'l)))
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
  :config
  (setq projectile-enable-caching t
        projectile-completion-system 'helm)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode 1))

(use-package rainbow-mode)

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t)
  (global-undo-tree-mode 1))

(use-package web-mode
  :mode ("\\.html?\\'" . web-mode)
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-opening t)
  (setq web-mode-enable-auto-pairing t))

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode 1)
  :config
  (setq which-key-idle-delay 1.0)
  :bind
  ("C-x w" . which-key-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))
;;; packages.el ends here
