;; cmd/alt
;; (setq ns-command-modifier 'meta)
;; (setq ns-alternate-modifier 'super)

;; rsense keys
(global-set-key (kbd "C-c C-c") 'ac-complete-rsense)
(global-set-key (kbd "C-c C-v") 'rsense-complete)

;; org mode keys
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; git keys
(global-set-key "\C-cg" 'magit-status)
(global-set-key "\C-cb" 'git-blame-mode)

;; copy-region-as-kill
(global-set-key (kbd "C-c C-y") 'copy-region-as-kill)
