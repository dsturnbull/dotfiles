;;;; magit hacks

;; commit 858cd16c0f3e3aace25750072f15a128f6fdaf49
;; Author: Phil Jackson <phil@shellarchive.co.uk>
;; Date:   Sat Feb 6 14:49:52 2010 +0000

(require 'magit)

(defun magit-insert-untracked-files ()
  (magit-git-section 'untracked "Untracked files:"
		     'magit-wash-untracked-files
		     "ls-files" "-t" "--others" "--exclude-standard" "--directory"))

(defun git-log-patch ()
  (interactive)
  (let
	  ((file-name (file-name-nondirectory buffer-file-name))
	   (dir-name (file-name-directory buffer-file-name)))
	(shell-command (concat "cd " dir-name "; git log -p " file-name))))

;; (apply 'make-comint buf-name "git-log" nil '("-p" "magit.el"))))
;; (comint-send-string (get-buffer-process (concat "*" buf-name "*"))
;;					(concat "git log -p " buffer-file-name "\n"))))
