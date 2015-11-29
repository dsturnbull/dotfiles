;; TODO
;; org-mode
;; hs-minor-mode -- ???
;; subword-mode -- camel case
;; M-s h -- highlights
;; winner-mode -- save window setup
;; C-x r l/b/m bookmarks
;; macros?
;; recursive editing
;; skeletons -- templating
;; hippie-expand
;; clang format
;; fix C function defs
;; alignment
;; projectile
;; all helm
;; mail
;; flush/keep
;; auto-revert-tail-mode
;; tramp

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#151718" "#CE4045" "#9FCA56" "#DCCD69" "#55B5DB" "#A074C4" "#55B5DB" "#D4D7D6"])
 '(ansi-term-color-vector
   [unspecified "#151718" "#CE4045" "#9FCA56" "#DCCD69" "#55B5DB" "#A074C4" "#55B5DB" "#D4D7D6"])
 '(bookmark-menu-length 32767)
 '(cursor-type (quote bar))
 '(custom-enabled-themes (quote (abyss)))
 '(custom-safe-themes
   (quote
    ("3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "c712d616ea5a9ef4e513681846eb908728bbb087c2d251ded8374ee9faafa199" "60bb7e2d647eda1ecab8bfe4afc21852d993b38a87ed50e4561bcf87c9adf84c" default)))
 '(custom-theme-directory "~/.emacs.d/themes")
 '(fringe-mode 6 nil (fringe))
 '(linum-format (quote dynamic))
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(windmove-wrap-around t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; basics
(setq-default ns-command-modifier 'meta
      ns-alternate-modifier 'super
      ns-function-modifier 'hyper
      ring-bell-function 'ignore
      inhibit-splash-screen t
      menu-bar-mode nil
      require-final-newline t
      scroll-bar-mode nil
      tool-bar-mode t
      tooltip-mode t)

;; remove pane scroll
(menu-bar-no-scroll-bar)

;; less turds
(setq backup-directory-alist `(("." . "~/.saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-version 2
      version-control t
      make-backup-files nil)

;; keys
(global-set-key (kbd "M-i") 'imenu)
(fset 'yes-or-no-p 'y-or-n-p)

;; helm
(require 'helm-config)

;; ido
(ido-mode 1)
(ido-everywhere t)
(ido-ubiquitous-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq org-completion-use-ido t)
(setq magit-completing-read-function 'magit-ido-completing-read)
(setq gnus-completing-read-function 'gnus-ido-completing-read)

;; smex
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "H-x") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; C-w kills line or region
(require 'whole-line-or-region)

;; save places
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/places")

;; hippie-expand
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; ugh hard copy
(setq-default indent-tabs-mode nil
              c-basic-indent 4
              c-basic-offset 4
              c-default-style "bsd")

;; pretty colours -- TODO didn't seem to do anything
(setq font-lock-mode t
      font-lock-maximum-decoration t
      global-font-lock-mode t)

;; zap -- df. zop -- dt.
(global-set-key [remap zap-to-char] 'zop-to-char)

;; delete trailing whitespace on save
(add-hook 'before-save-hook
	  'delete-trailing-whitespace)

;; use regex searches by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; utf
(prefer-coding-system 'utf-8)

;; view compressed files
(auto-compression-mode 1)

;; show matching parentheses
(show-paren-mode)

;; scrolling
(setq scroll-margin 1
      scroll-step 1
      scroll-preserve-screen-position t
      scroll-conservatively 10000
      redisplay-dont-pause t)

;; mwheel
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)

;; always autocomplete
(global-auto-complete-mode t)

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

;; locate
(setq locate-command "mdfind -name")

;; tiling

;; shrink, grow
(global-set-key (kbd "A-M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "A-M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "A-M-<down>") 'shrink-window)
(global-set-key (kbd "A-M-<up>") 'enlarge-window)

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

;; nyan
(nyan-mode t)
(nyan-start-animation)
(setq nyan-wavy-trail t)

;; projectile
(projectile-global-mode)
(add-hook 'projectile-mode-hook 'projectile-rails-on)

;; ruby
(add-hook 'ruby-mode-hook 'robe-mode)

;; scala
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; dired-x
(require 'dired-x)
