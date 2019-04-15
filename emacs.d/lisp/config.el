;;; config.el --- Various settings

;; UI
(setq initial-scratch-message nil)
(setq inhibit-splash-screen t)
(setq visible-bell t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Application
(setq auto-save-default nil)
(setq disabled-command-function nil)
(setq make-backup-files nil)
(setq org-log-done t)
(setq select-enable-clipboard t)
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode 1)
(recentf-mode 1)
(winner-mode 1)
(windmove-default-keybindings)
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-12"))

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
(global-hl-line-mode 1)
(global-subword-mode 1)
(show-paren-mode 1)
(transient-mark-mode 1)
(which-function-mode 1)

;; Hooks
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;;; config.el ends here
