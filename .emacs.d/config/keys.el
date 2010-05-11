;; rsense keys
(global-set-key (kbd "C-c C-c") 'ac-complete-rsense)
(global-set-key (kbd "C-c C-v") 'rsense-complete)

;; org mode keys
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; magit keys
(global-set-key "\C-cg" 'magit-status)

;; copy-region-as-kill
(global-set-key (kbd "C-c C-y") 'copy-region-as-kill)

;; handle shitty terminals
(define-key key-translation-map [?\C-h] [?\C-?])
