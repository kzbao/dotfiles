;;; General config

;; UI settings
(setq default-frame-alist
      '((width . 120)
        (height . 50)
        (font . "Hack-12")
        (alpha . (95 . 90))
        (ns-transparent-titlebar . t)
        (ns-appearance . dark)))

;; Remove unnecessary UI components
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Application settings
(setq auto-save-default nil
      custom-safe-themes t
      dired-recursive-copies 'always
      dired-recursive-deletes 'always
      disabled-command-function nil
      eshell-save-history-on-exit t
      inhibit-splash-screen t
      initial-scratch-message nil
      make-backup-files nil
      select-enable-clipboard t
      visible-bell t
      gc-cons-threshold (* 20 1024 1024))

(when (eq system-type 'darwin)
  (setq dired-use-ls-dired nil))

(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode 1)
(recentf-mode 1)
(winner-mode 1)
(windmove-default-keybindings)

;; Text editing
(setq auto-hscroll-mode 'current-line
      column-number-mode t
      line-number-mode t
      line-move-visual t
      require-final-newline t
      scroll-conservatively 1000
      scroll-margin 2
      scroll-preserve-screen-position 1
      search-whitespace-regexp "[-_ \t\n]+"
      show-paren-delay 0
      which-func-unknown "")

(setq-default indent-tabs-mode nil
              tab-width 8)

(delete-selection-mode 1)
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)
(global-subword-mode 1)
(transient-mark-mode 1)
(which-function-mode 1)
(show-paren-mode 1)

;; Hooks
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'config)
