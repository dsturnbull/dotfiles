;;;; rspec hax
(require 'rspec-mode)

;; TODO fork and push these minor changes

(defun rspec-run-single-file (spec-file &rest opts)
  "Runs spec with the specified options"
  (rspec-register-verify-redo (cons 'rspec-run-single-file (cons spec-file opts)))
  (compile (concat "spec \'" spec-file "\' " (mapconcat (lambda (x) x) opts " ")) t)
  (end-of-buffer-other-window 0))

(defun rspec-verify ()
  "Runs the specified spec, or the spec file for the current buffer."
  (interactive)
  (rspec-run-single-file (rspec-spec-file-for (buffer-file-name)) "--format specdoc" "--reverse" "--color"))

(defun rspec-verify-single ()
  "Runs the specified example at the point of the current buffer."
  (interactive)
  (rspec-run-single-file (rspec-spec-file-for (buffer-file-name)) "--format specdoc" "--reverse" "--color" (concat "--line " (number-to-string (line-number-at-pos)))))
