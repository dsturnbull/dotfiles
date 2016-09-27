(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;; less turds
(setq make-backup-files nil)

;; formatting
(setq c-basic-indent 4
      c-basic-offset 4
      tab-width 4)

;; pretty colours
(setq font-lock-mode t
      font-lock-maximum-decoration t
      global-font-lock-mode t)

;; niceties
(setq inhibit-splash-screen t
      require-final-newline t
      show-trailing-whitespace t
      vc-follow-symlinks t
      debug-on-error nil
      debug-on-signal nil
      ring-bell-function 'ignore)

;; use regex searches by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; disable shift-selection
(setq shift-select-mode nil)

;; delete trailing whitespace on save
(add-hook 'before-save-hook
	  'delete-trailing-whitespace)

;; y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; utf
(prefer-coding-system 'utf-8)

;; view compressed files
(auto-compression-mode 1)

;; unified diff mode
(setq diff-switches "-u")

;; remember buffer position
(setq save-place-file "~/.emacs.d/places")

;; fuzzy find-file
(ido-mode t)
(ido-ubiquitous-mode)
(ido-vertical-mode)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(global-set-key (kbd "C-x C-f") 'ido-find-file)

;; when you go over a parenthesis, the matching will highlight
(show-paren-mode)

;; appearance
(setq default-frame-alist
      '(
	(cursor-color . "Grey")
	(cursor-type . box)
	(foreground-color . "White")
	(background-color . "Black")
	(right-fringe . 0)
	(left-fringe . 0)
	(width . 150)
	(height . 80)
	;;(font . "terminus-12")
	;; alpha . (95 95))
	)
      )

;;(set-default-font "Terminus TTF-11")
;(setq system-uses-terminfo nil)
(set-face-attribute 'default nil
                    :family "Menlo" :height 120 :weight 'normal)

(setq scroll-bar-mode nil)

(blink-cursor-mode nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-no-scroll-bar)

;; scrolling
(setq scroll-margin 1
      scroll-step 1
      scroll-preserve-screen-position t)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed t)
(setq mouse-wheel-follow-mouse 't)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'alt)
(setq mac-function-modifier 'hyper)

;; always auto-complete
(global-auto-complete-mode t)
;;(set-cursor-color "white")
;;(setq ac-trigger-key 'TAB)

;; show search n/m
(global-anzu-mode +1)

;; time
(defface egoge-display-time
  '((((type x w32 mac))
     ;; #060525 is the background colour of my default face.
     (:foreground "#060525" :inherit bold))
    (((type tty))
     (:foreground "blue")))
  "Face used to display the time in the mode line.")
;; This causes the current time in the mode line to be displayed in
;; `egoge-display-time-face' to make it stand out visually.
(setq display-time-string-forms
      '((propertize (concat " " 24-hours ":" minutes " ")
 		    'face 'egoge-display-time)))
(display-time)

(setq locate-command "mdfind -name")

;; shrink, grow
(global-set-key (kbd "A-M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "A-M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "A-M-<down>") 'shrink-window)
(global-set-key (kbd "A-M-<up>") 'enlarge-window)

;; no tabs k
(setq-default indent-tabs-mode nil)

;; windmove
(windmove-default-keybindings)
(global-set-key (kbd "A-h") 'windmove-left)
(global-set-key (kbd "A-l") 'windmove-right)
(global-set-key (kbd "A-k") 'windmove-up)
(global-set-key (kbd "A-j") 'windmove-down)
(setq windmove-wrap-around t)

;; framemove
(framemove-default-keybindings)
(global-set-key (kbd "M-h") 'fm-left-frame)
(global-set-key (kbd "M-l") 'fm-right-frame)
(global-set-key (kbd "M-k") 'fm-up-frame)
(global-set-key (kbd "M-j") 'fm-down-frame)
;(global-set-key (kbd "M-n") 'make-frame)
(setq framemove-hook-into-windmove t)

;; rails
(projectile-global-mode)
(add-hook 'projectile-mode-hook 'projectile-rails-on)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-hook 'ruby-mode-hook 'robe-mode)

;; arg motion

;; fix ac keys
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; scala
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; ag

;; alt-tab
(global-set-key "\M-`" 'other-frame)

;; magit

;; guide-key
;; (setq guide-key/guide-key-sequence '(
;;                                      "C-x r"
;;                                      "C-x 4"
;;                                      "C-c p"
;;                                      "C-c r"
;;                                      ))
;; (guide-key-mode 1)

;; highlight-symbol
;(setq highlight-symbol-on-navigation t)

;(add-hook 'prog-mode-hook #'highlight-symbol-mode)
;(add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)

(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)

;; mu4e
(setq
 mu4e-maildir "~/Mail"
 mu4e-sent-folder "/iCloud/Sent Messages"
 mu4e-drafts-folder "/iCloud/Drafts"
 mu4e-trash-folder "/iCloud/Deleted Messages"
 mu4e-refile-folder "/iCloud/Archive"
;mu4e-get-mail-command "offlineimap"
 mu4e-get-mail-command "true"
 mu4e-use-fancy-chars t
 mu4e-attachment-dir "~/Desktop"
 mu4e-view-show-images t
 mu4e-update-interval 60)

(setq
 user-mail-address "david@broodax.net"
 user-full-name "David Turnbull"
 smtpmail-default-smtp-server "mail.broodax.net")


(defun use-icloud ()
  (setq message-send-mail-function 'smtpmail-send-it
        starttls-use-gnutls t
        smtpmail-starttls-credentials '(("smtp.mail.me.com" 587 nil nil))
        smtpmail-auth-credentials
        '(("smtp.mail.imap.com" 587 "dsturnbull" nil))
        smtpmail-default-smtp-server "smtp.mail.me.com"
        smtpmail-smtp-server "smtp.mail.me.com"
        smtpmail-smtp-service 587))

(defun use-arbor ()
  (setq message-send-mail-function 'smtpmail-send-it
        starttls-use-gnutls t
        smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
        smtpmail-auth-credentials
        '(("smtp.gmail.com" 587 "dturnbull" nil))
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587))

(defun use-broodax ()
  (setq message-send-mail-function 'smtpmail-send-it
        starttls-use-gnutls t
        smtpmail-starttls-credentials '(("mail.broodax.net" 587 nil nil))
        smtpmail-auth-credentials
        '(("smtp.mail.imap.com" 587 "david" nil))
        smtpmail-default-smtp-server "mail.broodax.net"
        smtpmail-smtp-server "mail.broodax.net"
        smtpmail-smtp-service 587))

(use-broodax)

(add-hook 'mu4e-compose-hook
          (defun my-set-from-address ()
            "Set the From address based on the To address of the original."
            (let ((msg mu4e-compose-parent-message)) ;; msg is shorter...
              (when msg
                (setq user-mail-address
                      (cond
                       ((mu4e-message-contact-field-matches msg :to "dsturnbull@me.com")
                        "dsturnbull@me.com")
                       ((mu4e-message-contact-field-matches msg :to "dturnbull@arbor.net")
                        "dturnbull@arbor.net")
                       (t "david@broodax.net")))
                (cond
                    ((mu4e-message-contact-field-matches msg :to "dsturnbull@me.com")
                      use-icloud)
                    ((mu4e-message-contact-field-matches msg :to "dturnbull@arbor.net")
                      use-arbor)
                    ((mu4e-message-contact-field-matches msg :to "david@broodax.net")
                      use-broodax))))))

;http://www.djcbsoftware.nl/code/mu/mu4e/Smart-refiling.html#Smart-refiling

(setq message-kill-buffer-on-exit t)
(setq mail-user-agent 'mu4e-user-agent)

;; twitter
(setq twittering-use-master-password t)

;; which-key
(which-key-mode)

;; aggressive indent
(add-hook 'emacs-lisp-moed-hook #'aggressive-indent-mode)
(add-hook 'css-mode-hook #'aggressive-indent-mode)

;; o, O

(defun vi-open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun vi-open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))

(global-set-key (kbd "C-o") 'vi-open-line-below)
(global-set-key (kbd "C-S-o") 'vi-open-line-above)

;; yy p
(defun yank-whole-line ()
  (interactive)

  (kill-whole-line)
  (yank))

(global-set-key (kbd "C-S-y") 'yank-whole-line)

(defun yyp ()
  (interactive)
  (yank-whole-line)
  (forward-line -1)
  (yank))

  (global-set-key (kbd "C-S-p") 'yyp)

(nyan-mode t)
(nyan-start-animation)
(setq nyan-wavy-trail t)

;; helm

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-i") 'helm-semantic-or-imenu)
;;(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x r l") 'helm-filtered-bookmarks)
(global-set-key (kbd "C-:") 'ac-complete-with-helm)
(global-set-key (kbd "C-c t") 'helm-cmd-t)

(setq helm-mode-fuzzy-match t)
(setq helm-completion-in-region-fuzzy-match t)
(setq projectile-completion-system 'helm)
(setq projectile-switch-project-action 'helm-projectile)
(helm-projectile-on)
(helm-autoresize-mode 1)

;; resolve buffer to filename, insert into kill ring
(defun replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

(defun copy-full-path-to-kill-ring ()
  "copy buffer's full path to kill ring"
  (interactive)
  (when buffer-file-name
    ;;(kill-new (replace-in-string (projectile-project-root) "" (file-truename buffer-file-name)))))
    ;;(kill-new (file-truename buffer-file-name))))
    (kill-new (concat
                (replace-in-string (projectile-project-root) "" (file-truename buffer-file-name))
                ":"
                (number-to-string (line-number-at-pos))))))

(global-set-key (kbd "C-c C-f") 'copy-full-path-to-kill-ring)

;; c
(defun my-c-setup ()
  (c-set-offset 'case-label 0))
(add-hook 'c-mode-hook 'my-c-setup)
(add-hook 'c-mode-hook 'semantic-mode)

;; org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; sigh, PATH
(setenv "PATH"
        (concat
         "/usr/local/bin" ":"
         (getenv "PATH")))

;; irony
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async)

  (flycheck-mode))

(defun my-ac-irony-setup ()
  ;;(yas-minor-mode 1)
  (auto-complete-mode 1)
  (add-to-list 'ac-sources 'ac-source-irony)
  (define-key irony-mode-map (kbd "M-RET") 'ac-complete-irony-async))

(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'my-ac-irony-setup)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(add-to-list 'load-path "~/.emacs.d/vendor/")
(require 'ac-irony)

;; javascript
(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)
                      '(emacs-lisp-checkdoc)
                      ))
(flycheck-add-mode 'javascript-eslint 'web-mode)

;;(setq-default flycheck-disabled-checkers
;;              (append flycheck-disabled-checkers
;;                      ('json-jsonlist)))

(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook 'my-web-mode-hook)

;; hideshow
(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))

(defun toggle-hiding (column)
  (interactive "P")
  (if hs-minor-mode
      (if (condition-case nil
              (hs-toggle-hiding)
            (error t))
          (hs-show-all))
    (toggle-selective-display column)))

(global-set-key (kbd "C-+") 'toggle-hiding)
(global-set-key (kbd "C-\\") 'toggle-selective-display)

(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)

;; C-c C-c
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(global-set-key (kbd "C-;") 'comment-or-uncomment-region-or-line)

;; elscreen
(elscreen-start)
(setq elscreen-display-tab nil)

;; elixir
(require 'elixir-mode)
(add-to-list 'elixir-mode-hook
             (defun auto-activate-ruby-end-mode-for-elixir-mode ()
               (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
                    "\\(?:^\\|\\s-+\\)\\(?:do\\)")
               (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
               (ruby-end-mode +1)))
