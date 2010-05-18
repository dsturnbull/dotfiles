;; javascript stuff
(defun my-espresso-mode-hook ()
  (setq espresso-indent-level 2
		indent-tabs-mode nil
		c-basic-offset 2))

(add-hook 'espresso-mode-hook 'my-espresso-mode-hook)
