;; flymake
(require 'flymake)
(require 'flymake-ruby)
(require 'flymake-nicey)

(my-flymake-minor-mode)

(load-file (concat vendor-dir "nxhtml/related/flymake-js.el"))
(setq flymake-js-rhino-jar (expand-file-name "~/src/rhino/js.jar"))
(setq flymake-js-rhino-use-jslint t)
(flymake-js-load)

(add-hook 'espresso-mode-hook 'flymake-js-init)
(add-hook 'find-file-hook 'flymake-find-file-hook)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
