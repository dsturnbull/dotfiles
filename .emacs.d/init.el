;; base
(setq dotfiles-dir "~/.emacs.d/")
(setq vendor-dir (concat dotfiles-dir "vendor/"))
(setq theme-dir (concat dotfiles-dir "themes/"))

;; load path
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path vendor-dir)
(add-to-list 'load-path theme-dir)

;; env path
(require 'dave-path)

;; package initialisers
(require 'dave-elpa)
(require 'dave-auto-complete)
(require 'dave-flymake)
(require 'dave-icicles)
(require 'dave-mode-compile)
(require 'dave-cedet)
(require 'dave-agenda)
(require 'dave-rsense)
(require 'dave-ctags)
(require 'dave-jabber)
(require 'dave-nxhtml)
(require 'dave-javascript)
(require 'dave-ruby)
(require 'dave-rspec-mode)
(require 'dave-rinari)
(require 'dave-magit)
(require 'dave-themes)
(require 'dave-yaml)
(require 'dave-haml)

;; load libs - probably going to make initializers for these
(require 'fuzzy-match)
(require 'auto-complete)
(require 'rsense)
(require 'jabber)

;; local config
(require 'dave-defaults)
(require 'dave-appearance)
(require 'dave-keys)
(require 'dave-misc)
