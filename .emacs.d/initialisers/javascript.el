;; javascript stuff
(defun my-js-mode-hook ()
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (setq espresso-indent-level 4))

(add-hook 'js-mode-hook 'my-js-mode-hook)
