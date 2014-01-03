;; allow shift-arrows for windmove
(setq org-replace-disputed-keys t)

;; delete trailing whitespace on save
(add-hook 'before-save-hook
		  'delete-trailing-whitespace)

;; y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; utf
(prefer-coding-system 'utf-8)

;; window manipulation
(winner-mode 1)

;; view compressed files
(auto-compression-mode 1)

;; window movement
(windmove-default-keybindings)

;; unified diff mode
(setq diff-switches "-u")

;; remember buffer position
(custom-set-variables
 '(save-place t nil (saveplace)))
(setq save-place-file (concat dotfiles-dir "places"))

;; fuzzy find-file
(ido-mode t)

;; just load it, for viper-join-lines
(setq viper-mode nil)
(require 'viper)

;; when you go over a parenthesis, the matching will highlight
(show-paren-mode)

;; git-blame-mode
(if (file-exists-p "/opt/local/share/doc/git-core/contrib/emacs/git-blame.el")
	(load-file "/opt/local/share/doc/git-core/contrib/emacs/git-blame.el"))

;; yegge-mode
(load-file (concat vendor-dir "yegge.el"))
