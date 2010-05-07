;; mode-compile
(add-to-list 'load-path (concat vendor-dir "mode-compile.el"))

(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
;; (global-set-key "\C-cc" 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
;; (global-set-key "\C-ck" 'mode-compile-kill)

(provide 'dave-mode-compile)