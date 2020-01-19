(setq first-marker (make-marker))
(setq second-marker (make-marker))
      
(defun emacs-transclusion/get-file-contents (path)
  "Returns content of file as string"
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun emacs-transclusion/find-embed-syntax-in-current-buffer ()
    (let ((buffer-point (re-search-forward "\\[EMBED: \\([^\\]*\\)\\(]\\)")))
      (when (match-string 0)
	  
	;re-search-forward moves point to end of match.
	;save position
	(setq first-marker (point))

	(insert (emacs-transclusion/get-file-contents (match-string 1)))
	(setq second-marker (point))

	;EMACS BUG! Can't use insert file, file marker are not moved when text is inserted

        ;(insert-file-contents-literally (match-string  1))

      )
    )
)


(with-current-buffer "testbuffer"
    ;(delete-region first-marker second-marker)
    (goto-char 1)
    (emacs-transclusion/find-embed-syntax-in-current-buffer)
    (delete-region first-marker second-marker)
)


