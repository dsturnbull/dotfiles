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
(global-set-key "\C-cp" 'git-log-patch)

;; copy-region-as-kill
(global-set-key (kbd "C-c C-y") 'copy-region-as-kill)

;; recompile
(global-set-key (kbd "C-c r") 'recompile)

;; ri
(global-set-key (kbd "C-h r") 'ri)

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "M-/") 'hippie-expand)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; ok, let's see
(global-set-key (quote [tab]) 'smart-tab)
(defun undo-warning ()
  (interactive)
  (message "ni!"))
(global-set-key (kbd "s-z") 'undo-warning)
