(setq first-marker (make-marker))
(setq second-marker (make-marker))

(setq emacs-transclusion/overlays '())

(setq emacs-transclusion/refernece-display-state nil)

(defun emacs-transclusion/toggle-display-embed-syntax ()
  (interactive)

  "Toggles displaying of embed syntax in documents."

  ;;Inverse turthyness of reference display state
  (setq emacs-transclusion/refernece-display-state (not emacs-transclusion/refernece-display-state))
  (dolist (cur-overlay emacs-transclusion/overlays)
    (overlay-put cur-overlay 'invisible emacs-transclusion/refernece-display-state)))

(defun emacs-transclusion/find-embed-syntax-in-current-buffer ()
  (let ((buffer-point (re-search-forward "\\[EMBED: \\([^\\]*\\)\\(]\\)"))
	(overlay-ref nil))
      (when (match-string 0)


	(setq overlay-ref (make-overlay (match-beginning 0) (match-end 0)))
	(push overlay-ref emacs-transclusion/overlays)

	;re-search-forward moves point to end of match.
	;save position
	(setq first-marker (point-marker))

        (forward-char)
	(setq second-marker (point-marker))
        (backward-char)
        (insert-file-contents (match-string 1))))
)


(with-current-buffer "testbuffer"
    ;(delete-region first-marker second-marker)
    (goto-char 1)
    (emacs-transclusion/find-embed-syntax-in-current-buffer)
    ;(delete-region first-marker second-marker)
)

;(add-hook 'before-save-hook 'emacs-transclusion/before-save-hook)

;(defun emacs-transclusion/before-save-hook ()
;  (insert "hello world!"))
