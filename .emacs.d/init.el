;; base
(setq dotfiles-dir "~/.emacs.d/")
(setq vendor-dir (concat dotfiles-dir "vendor/"))
(setq theme-dir (concat dotfiles-dir "themes/"))
(setq initialiser-dir (concat dotfiles-dir "initialisers/"))
(setq config-dir (concat dotfiles-dir "config/"))

(defun add-initialiser-prefix (path)
  (concat initialiser-dir path))

(defun add-config-prefix (path)
  (concat config-dir path))

;; load path
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path vendor-dir)
(add-to-list 'load-path theme-dir)

;; package initialisers
(mapc #'load (mapcar 'add-initialiser-prefix (directory-files initialiser-dir nil ".*el$")))

;; local config
(mapc #'load (mapcar 'add-config-prefix (directory-files config-dir nil ".*el$")))
