;; cucumber
(add-to-list 'load-path (concat vendor-dir "cucumber"))
(require 'feature-mode)

(defun feature-run-real-cucumber (cuke-opts feature-file)
  "Runs cucumber with the specified options"
  (feature-register-verify-redo (list 'feature-run-cucumber
                                      (list 'quote cuke-opts)
                                      feature-file))
  ;; redoer is registered

  (let ((opts-str    (mapconcat 'identity cuke-opts " "))
        (feature-arg (if feature-file
                         (concat " '" feature-file "'")
                       "")))
    (ansi-color-for-comint-mode-on)
    (compile (concat "cucumber " opts-str feature-arg) t))
  (end-of-buffer-other-window 0))

(defun feature-verify-scenario-at-pos ()
  "Run the scenario defined at pos.  If post is not specified the current buffer location will be used."
  (interactive)
  (feature-run-real-cucumber
   ""
   (concat (buffer-file-name) ":" (number-to-string (line-number-at-pos)))))

(defun feature-verify-all-scenarios-in-buffer ()
  "Run all the scenarios defined in current buffer."
  (interactive)
  (feature-run-real-cucumber '() (buffer-file-name)))
