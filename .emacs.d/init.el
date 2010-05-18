;; base
(setq dotfiles-dir (expand-file-name "~/.emacs.d/"))
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

;; FIXME move to erc.el
(custom-set-variables
 '(erc-email-userid "dave")
 '(erc-nick "dave")
 '(erc-port 6667)
 '(erc-prompt-for-password nil)
 '(erc-send-whitespace-lines nil)
 '(erc-server "irc.meobets.com")
 '(erc-server-reconnect-attempts 86400)
 '(erc-server-reconnect-timeout 10)
 '(erc-user-full-name "David Turnbull")
 '(erc-warn-about-blank-lines nil)
 '(erc-whowas-on-nosuchnick t))
