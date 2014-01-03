;;; columnify-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (columnify) "columnify" "columnify.el" (19428 15955))
;;; Generated autoloads from columnify.el

(autoload 'columnify "columnify" "\
Reformat the lines in the current region into a columnar list.

Choose the number of columns based on the width of the longest
line and the current value of `fill-column'.  Try to get at least
a half-column-width of space between each column.

The items in the region are taken to be in \"column-major\"
order: reading down each column, starting with the leftmost
column, yields the items in the order they originally appeared.

Ignore whitespace at the beginning and end of each line.

This command assumes that all characters have a width that is an
even multiple of the width of a space.

\(fn START END)" t nil)

;;;***

;;;### (autoloads nil nil ("columnify-pkg.el") (19428 15955 67145))

;;;***

(provide 'columnify-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; columnify-autoloads.el ends here
