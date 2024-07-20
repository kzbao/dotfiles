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

(eval-when-compile
  (require 'use-package))

;; Use package config
(setq use-package-always-ensure t
      use-package-always-defer nil
      use-package-verbose t
      use-package-compute-statistics t)

(use-package ace-window
  :bind ([remap other-window] . ace-window))

(use-package aggressive-indent
  :bind ("C-c a" . aggressive-indent-mode))

(use-package company
  :demand t
  :init
  (setq company-minimum-prefix-length 1
        company-idle-delay (lambda () (if (company-in-string-or-comment) nil 2))
        company-selection-wrap-around t
        company-transformers '(company-sort-by-occurrence
                               company-sort-by-backend-importance)
        company-show-quick-access 'left)
  :bind
  (:map company-active-map
        ("RET" . nil)
        ("<return>" . nil)
        ("<tab>" . company-complete-common-or-cycle)
        ("C-<return>" . company-complete)
        ("C-h" . company-quickhelp-manual-begin))
  :config
  (global-company-mode))

(use-package company-quickhelp
  :config
  (setq company-quickhelp-delay nil)
  (company-quickhelp-mode 1))

(use-package consult
  :bind
  (("C-x b" . consult-buffer)
   ("C-x p b" . consult-project-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x r b" . consult-bookmark)
   ("M-y" . consult-yank-pop)
   ("M-g g" . consult-goto-line)
   ("M-g o" . consult-outline)
   ("M-g i" . consult-imenu)
   ("M-s d" . consult-find)
   ("M-s a" . consult-ripgrep)
   ("M-s s" . consult-line)
   ("M-s b" . consult-line-multi)
   ("M-s e" . consult-isearch-history)
   :map isearch-mode-map
   ("M-s e" . consult-isearch-history)
   ("M-s s" . consult-line)
   ("M-s l" . consult-line-multi)
   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history)))

(use-package diminish)

(use-package doom-themes
  :config
  (doom-themes-org-config)
  (load-theme 'doom-dark+ t))

(use-package easy-kill
  :bind ([remap kill-ring-save] . easy-kill))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package eglot
  :ensure t
  :hook ((python-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (eglot-managed-mode . (lambda () (company-mode))))
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(js-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(typescript-mode . ("typescript-language-server" "--stdio"))))

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

(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package org
  :config
  (setq org-confirm-babel-evaluate nil
        org-src-preserve-indentation t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (java . t)
     (js . t)
     (python . t)
     (ruby . t))))

(use-package powerline
  :config
  (setq powerline-default-separator 'slant)
  (defface powerline-set1 '((t (:background "#023F7B" :foreground "#d4d4d4" :inherit mode-line)))
    "Blue with white text")
  (defface powerline-set2 '((t (:background "#313131" :foreground "#1c1c1c" :inherit mode-line)))
    "Grey with multicolor text")
  (defface powerline-set3 '((t (:background "#121212" :foreground "#35CDAF" :inherit mode-line)))
    "Black with teal text")
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

(use-package rainbow-mode)

(use-package savehist
  :init
  (savehist-mode))

(use-package typescript-mode)

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

(use-package vertico
  :init
  (vertico-mode))

(use-package vundo
     :bind
     ("C-c u" . vundo))

(use-package which-key
  :config
  (which-key-mode))

(use-package yaml-mode
  :mode "\\.yml\\'")

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  :bind
  (("C-c y e" . yas-expand)
   ("C-c y n" . yas-new-snippet)
   ("C-c y v" . yas-visit-snippet-file)
   ("C-c y r" . yas-reload-all)
   ("C-c y i" . yas-insert-snippet)
   ("C-c y l" . yas-describe-tables)))

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'packages)
