(use-package projectile-rails
  :ensure t
  :config
  (projectile-rails-global-mode)
  (define-key projectile-rails-mode-map (kbd "C-c r") 'projectile-rails-command-map))

(use-package terraform-mode
  :ensure t)
