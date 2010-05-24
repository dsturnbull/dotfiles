;; flymake
(require 'flymake)
(require 'flymake-ruby)
(require 'flymake-nicey)

;; initiate it now so it's loaded every time
(my-flymake-minor-mode)

;; register jaml as wanting jslint
(eval-after-load "flymake-js"
  '(progn
     (add-to-list 'flymake-allowed-js-file-name-masks
		  '("\\.jaml\\'" flymake-js-init))))

(load-file (concat vendor-dir "nxhtml/related/flymake-js.el"))
(setq flymake-js-engine 'jslintpy)
(flymake-js-load)

(add-hook 'espresso-mode-hook 'flymake-js-init)
(add-hook 'find-file-hook 'flymake-find-file-hook)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

