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
(fset 'yes-or-no-p 'y-or-n-p)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(setq disabled-command-function nil)

;; Text editing settings
(add-to-list 'default-frame-alist
             '(font . "Source Code Pro-12"))
(setq-default indent-tabs-mode nil)
(setq column-number-mode t
      line-number-mode t)
(setq require-final-newline t)
(setq auto-hscroll-mode 'current-line)
(setq line-move-visual t)
(setq scroll-margin 2
      scroll-preserve-screen-position 1)
(setq search-whitespace-regexp "[-_ \t\n]+")
(delete-selection-mode 1)
(electric-pair-mode 1)
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)
(transient-mark-mode 1)
(show-paren-mode 1)
(setq show-paren-delay 0)
(global-subword-mode 1)
(which-function-mode 1)
(setq which-func-unknown "")
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Basic language settings
(setq js-indent-level 2)
(setq css-indent-level 2)