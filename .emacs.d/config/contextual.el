;; contextual - system name, user, system os

;; Work around a 'bug' on OS X where system-name is FQDN
(if (eq system-type 'darwin)
    (setq system-name (car (split-string system-name "\\."))))

(setq system-specific-config (concat dotfiles-dir system-name ".el"))

(if (eq system-type 'darwin)
	(if (file-exists-p (concat dotfiles-dir "darwin.el"))
		(load (concat dotfiles-dir "darwin.el"))))

(if (eq system-type 'gnu/linux)
	(if (file-exists-p (concat dotfiles-dir "linux.el"))
		(load (concat dotfiles-dir "linux.el"))))

(if (file-exists-p system-specific-config) (load system-specific-config))
