(use-package projectile-rails
  :config
  (projectile-rails-global-mode)
  (define-key projectile-rails-mode-map (kbd "C-c r") 'projectile-rails-command-map))

(use-package rspec-mode)

(use-package ruby-end)

(use-package slim-mode)

(use-package terraform-mode)
