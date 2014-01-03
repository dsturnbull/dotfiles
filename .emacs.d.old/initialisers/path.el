;;;; exec path
;; FIXME it should be in its own initialiser
(setenv "SHELL" "/bin/zsh")
(setq shell-file-name "/bin/zsh")

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell
      (replace-regexp-in-string "[[:space:]\n]*$" ""
        (shell-command-to-string "$SHELL -l -c 'echo $PATH' 2>/dev/null"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))
