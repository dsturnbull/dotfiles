;; flymake
(require 'flymake)

(add-hook 'find-file-hook 'flymake-find-file-hook)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

(provide 'dave-flymake)
