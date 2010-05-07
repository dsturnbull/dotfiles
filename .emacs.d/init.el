;;;; base
(add-to-list 'load-path "~/.emacs.d")

;;;; elpa
(when
	(load
	 (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;;;; starting appearance
(set-default-font "menlo-9")
(setq default-frame-alist
      '(
		(cursor-color . "Grey")
		(cursor-type . box)
		(foreground-color . "White")
		(background-color . "Black")
		(right-fringe . 0)
		(left-fringe . 0)
		))
(menu-bar-no-scroll-bar)

;;;; fuzzy match
(load "fuzzy-match.el")

;;;; icicles
(add-to-list 'load-path "~/.emacs.d/icicles")
(require 'icicles)
;; not just yet actually
;; (icy-mode 1)

;;;; defaults
(setq-default
 make-backup-files nil ;; stop shitting in my filesystem

 ;; formatting
 c-basic-indent 4
 c-basic-offset 4
 tab-width 4

 ;; pretty colours
 font-lock-mode t
 font-lock-maximum-decoration t
 global-font-lock-mode t

 ;; niceties
 inhibit-splash-screen t
 require-final-newline t
 scroll-bar-mode nil
 show-trailing-whitespace t
 vc-follow-symlinks t
 visible-bell t
 )

;;;; allow shift-arrows for windmove
(setq org-replace-disputed-keys t)

;;;; delete trailing whitespace on save
(add-hook 'before-save-hook
		  'delete-trailing-whitespace)

;;;; y/n
(fset 'yes-or-no-p 'y-or-n-p)

;;;; utf
(prefer-coding-system 'utf-8)

;;;; window manipulation
(winner-mode 1)

;;;; view compressed files
(auto-compression-mode 1)

;;;; window movement
(windmove-default-keybindings)

;;;; mode-compile
(add-to-list 'load-path "~/.emacs.d/mode-compile.el")
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
;; (global-set-key "\C-cc" 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
;; (global-set-key "\C-ck" 'mode-compile-kill)

;;;; cedet
;(load-file "~/.emacs.d/cedet/common/cedet.el")
;(require 'semantic-ia)
;(require 'semantic-gcc)
;; for reference:
;; (semantic-add-system-include "~/exp/include/boost_1_37" 'c++-mode)
;(semantic-load-enable-code-helpers)
;(semantic-load-enable-gaudy-code-helpers)
;(semantic-load-enable-excessive-code-helpers)

;;;; auto-complete
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete)

;;;; rsense
(setq rsense-home "/Users/dave/.emacs.d/rsense")
(add-to-list 'load-path (concat rsense-home "/etc"))
(require 'rsense)

(global-set-key (kbd "C-c C-c") 'ac-complete-rsense)
(global-set-key (kbd "C-c C-v") 'rsense-complete)

;;;; agena files
(setq org-agenda-files (list
						"~/org/uni.org"
						"~/org/todo.org"
						"~/org/spacemmo.org"
						"~/org/emacs.org"
						))

;;;; write agenda to orgmode.ics which is subscribed to in iCal
(setq org-combined-agenda-icalendar-file "~/Sites/orgmode.ics")

;;;; org mode key bindings
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;;; org mode workflow
(setq org-todo-keywords
	  '((sequence
		 "TODO(t)"
		 "ACK(a)"
		 "CUR(c)"
		 "VERIFY(v)"
		 "DELEGATED(g)"
		 "BACKLOG(b)"
		 "|"
		 "DONE(d@/!)"
		 "WONTFIX(w!@)"
		 ))
	  )

;;;; unified diff mode
(setq diff-switches "-u")

;;;; remember buffer position
(custom-set-variables
 '(save-place t nil (saveplace)))

;;;; fuzzy find-file
(ido-mode t)

;;;; ctags
(setq path-to-ctags "/opt/local/bin/ctags")
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f %s/TAGS -e -R %s" path-to-ctags dir-name dir-name)))

;;;; jabber
(add-to-list 'load-path "~/.emacs.d/jabber")
(require 'jabber)
