;;; config.el --- Various settings

;; Remove unnecessary UI components
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Application settings
(setq auto-save-default nil)
(setq custom-safe-themes t)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq disabled-command-function nil)
(setq eshell-save-history-on-exit t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(setq make-backup-files nil)
(setq select-enable-clipboard t)
(setq visible-bell t)

(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode 1)
(recentf-mode 1)
(winner-mode 1)
(windmove-default-keybindings)

(add-to-list 'default-frame-alist '(width  . 120))
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(font . "Hack-12"))
(add-to-list 'default-frame-alist '(alpha . (95 . 90)))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Text editing
(setq auto-hscroll-mode 'current-line)
(setq column-number-mode t)
(setq line-number-mode t)
(setq line-move-visual t)
(setq require-final-newline t)
(setq scroll-conservatively 1000)
(setq scroll-margin 2)
(setq scroll-preserve-screen-position 1)
(setq search-whitespace-regexp "[-_ \t\n]+")
(setq show-paren-delay 0)
(setq which-func-unknown "")

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

(delete-selection-mode 1)
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)
(global-subword-mode 1)
(transient-mark-mode 1)
(which-function-mode 1)
(show-paren-mode 1)

;; Hooks
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'config)
;;; config.el ends here
