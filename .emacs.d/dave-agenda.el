;; agena files
(setq org-agenda-files (list
						"~/org/uni.org"
						"~/org/todo.org"
						"~/org/spacemmo.org"
						"~/org/emacs.org"
						))

;; write agenda to orgmode.ics which is subscribed to in iCal
;; (setq org-combined-agenda-icalendar-file "~/Sites/orgmode.ics")

;; org mode workflow
(setq org-todo-keywords
	  '((sequence
		 "TODO(t)"
		 "ACK(a)"
		 "CUR(c)"
		 "VERIFY(v)"
		 "DELEGATED(g)"
		 "BACKLOG(b)"
		 "|"
		 "DONE(d@/!)"
		 "WONTFIX(w!@)"
		 ))
	  )

(provide 'dave-agenda)
