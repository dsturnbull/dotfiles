;;; ruby-guard.el --- Launching guard directly inside emacs.

;; Copyright (C) 2014 Zhang Kai Yu

;; Author: Zhang Kai Yu <yeannylam@gmail.com>
;; Version: 0.1.0
;; Package-Version: 20141218.1914
;; Keywords: ruby, guard, rails
;; URL: https://github.com/cheunghy/ruby-guard

;;; Commentary:

;; M-x ruby-guard to launch.

;;; Code:

(defvar ruby-guard-buffer-name "*guard*")

(defun ruby-guard-root (&optional last-directory)
  "Return the directory name where guard file is located."
  (locate-dominating-file (or last-directory
                              (file-truename default-directory))
                          "Guardfile"))

(defun ruby-guard-spring-p ()
  (file-exists-p (file-truename
                  (concat temporary-file-directory
                          "spring/"
                          (md5 (ruby-guard-root) 0 -1)
                          ".pid"))))

(defun ruby-guard-zeus-p ()
  (file-exists-p (expand-file-name ".zeus.sock" (ruby-guard-root))))

(defun ruby-guard-bundle-p ()
  (file-exists-p (expand-file-name "Gemfile" (ruby-guard-root))))

(defun ruby-guard-command-name ()
  (cond ((ruby-guard-spring-p)
         "spring guard")
        ((ruby-guard-bundle-p)
         "bundle exec guard")
        (t "guard")))

(defmacro ruby-guard-with-root (body-form)
  `(let ((default-directory (ruby-guard-root)))
     ,body-form))

;;;###autoload
(defun ruby-guard ()
  (interactive)
  (let ((default-directory (ruby-guard-root)))
    (if default-directory
        (progn
          (if (member ruby-guard-buffer-name
                      (mapcar 'buffer-name (buffer-list)))
              (switch-to-buffer ruby-guard-buffer-name)
            (ruby-guard-with-root
             (async-shell-command
              (ruby-guard-command-name)
              (get-buffer-create ruby-guard-buffer-name)))))
      (error "Cannot find Guardfile."))))

(provide 'ruby-guard)

;;; ruby-guard.el ends here
