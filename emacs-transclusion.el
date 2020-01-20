(setq first-marker (make-marker))
(setq second-marker (make-marker))

(setq emacs-transclusion/overlays '())

(setq emacs-transclusion/refernece-display-state nil)

(defun emacs-transclusion/toggle-display-embed-syntax ()

  "Toggles displaying of embed syntax in documents."

  ;;Inverse turthyness of reference display state
  (setq emacs-transclusion/refernece-display-state (not emacs-transclusion/refernece-display-state))
  (dolist (cur-overlay emacs-transclusion/overlays)
    (overlay-put cur-overlay 'invisible emacs-transclusion/refernece-display-state)))


(defun emacs-transclusion/get-file-contents (path)
  "Returns content of file as string"
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun emacs-transclusion/find-embed-syntax-in-current-buffer ()
  (let ((buffer-point (re-search-forward "\\[EMBED: \\([^\\]*\\)\\(]\\)"))
	(overlay-ref nil))
      (when (match-string 0)


	(setq overlay-ref (make-overlay (match-beginning 0) (match-end 0)))
	(push overlay-ref emacs-transclusion/overlays)

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

