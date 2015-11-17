(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;; less turds
(setq backup-directory-alist `(("." . "~/.saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-version 2
      version-control t
      make-backup-files nil)

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

;; smex
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

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
	;;(font . "terminus-12")
	;; alpha . (95 95))
	)
      )

;;(set-default-font "Terminus TTF-11")
(setq system-uses-terminfo nil)
(set-face-attribute 'default nil
                    :family "Menlo" :height 100 :weight 'normal)

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
(require 'auto-complete)
(global-auto-complete-mode t)
;;(set-cursor-color "white")
;;(setq ac-trigger-key 'TAB)

;; show search n/m
(global-anzu-mode +1)

;; keep history of saves
(require 'backup-each-save)
(add-hook 'after-save-hook 'backup-each-save)

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
(setq windmove-wrap-around t)

;; evil leader
(require 'evil-leader)
(global-evil-leader-mode)
;; (evil-leader/set-leader ",")
;; (evil-leader/set-key
;;   "e" 'find-file
;;   "b" 'switch-to-buffer)
;; (evil-leader/set-key-for-mode 'emacs-lisp-mode "b" 'byte-compile-file)

;; evil!
(require 'evil)
(evil-mode 1)

;; evil number incrementing
(require 'evil-numbers)
(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; mark replace, not sure this is useful
(require 'evil-mark-replace)

;; commentary
(evil-commentary-mode)

;; easymotion
(evilem-default-keybindings "SPC")

;; rails
(projectile-global-mode)
(add-hook 'projectile-mode-hook 'projectile-rails-on)
(require 'mmm-mode)
(require 'rhtml-mode)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(require 'haml-mode)
(require 'yaml-mode)
(require 'rspec-mode)
(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)
(evil-define-key 'normal robe-mode-map (kbd "M-.") 'robe-jump)
(require 'evil-rails)

;; arg motion
(require 'evil-args)

;; bind evil-args text objects
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

;; bind evil-forward/backward-args
(define-key evil-normal-state-map "L" 'evil-forward-arg)
(define-key evil-normal-state-map "H" 'evil-backward-arg)
(define-key evil-motion-state-map "L" 'evil-forward-arg)
(define-key evil-motion-state-map "H" 'evil-backward-arg)

;; bind evil-jump-out-args
(define-key evil-normal-state-map "K" 'evil-jump-out-args)

;; rsi
(evil-rsi-mode)

;; fix ac keys
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; scala
(require 'scala-mode2)
(require 'sbt-mode)
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; ag
(require 'ag)
