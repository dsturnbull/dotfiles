;; contextual - system name, user, system os

;; Work around a 'bug' on OS X where system-name is FQDN
(if (eq system-type 'darwin)
    (setq system-name (car (split-string system-name "\\."))))

(setq system-specific-config (concat dotfiles-dir system-name ".el"))

;; FIXME disable this until i can figure out how to turn system-type into a string
;; system-type-config (concat dotfiles-dir system-type ".el")

(if (file-exists-p system-specific-config) (load system-specific-config))

