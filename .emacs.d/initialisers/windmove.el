;; make it shut up

(eval-after-load 'windmove
  '(progn
	 ;; Selects the window that's hopefully at the location returned by
	 ;; `windmove-other-window-loc', or screams if there's no window there.
	 ;; Also shut the fuck up.
	 (defun windmove-do-window-select (dir &optional arg window)
	   "Move to the window at direction DIR.
DIR, ARG, and WINDOW are handled as by `windmove-other-window-loc'.
If no window is at direction DIR, an error is signaled."
	   (let ((other-window (windmove-find-other-window dir arg window)))
		 (cond ((null other-window)
				(message "No window %s from selected window" dir))
			   ((and (window-minibuffer-p other-window)
					 (not (minibuffer-window-active-p other-window)))
				(message "Minibuffer is inactive"))
			   (t
				(select-window other-window)))))))

