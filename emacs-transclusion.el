(setq first-marker (make-marker))
(setq second-marker (make-marker))

(setq emacs-transclusion/overlays '())

(setq emacs-transclusion/refernece-display-state nil)

(setq emacs-transclusion/embed-syntax-regex "\\[EMBED: \\([^\\]*?\\)\\(]\\)")

(defun emacs-transclusion/toggle-display-embed-syntax ()
  (interactive)

  "Toggles displaying of embed syntax in documents."

  ;;Inverse turthyness of reference display state
  (setq emacs-transclusion/refernece-display-state (not emacs-transclusion/refernece-display-state))
  (dolist (cur-overlay emacs-transclusion/overlays)
    (overlay-put cur-overlay 'invisible emacs-transclusion/refernece-display-state)))

(defun emacs-transclusion/transclude-data-for-current-buffer ()
  "Finds embed references in buffer, and transcludes data into buffer."

  (let ((buffer-point (re-search-forward emacs-transclusion/embed-syntax-regex))
	(overlay-ref nil))
      (when (match-string 0)
	(setq overlay-ref (make-overlay (match-beginning 0) (match-end 0)))
	(push overlay-ref emacs-transclusion/overlays)

	;re-search-forward moves point to end of match.
	;save position
	(setq first-marker (point-marker))
        (insert " ")
	(setq second-marker (point-marker))
        (backward-char)
        (insert-file-contents (match-string 1))
        (goto-char second-marker)
        (backward-char)
        (delete-char 1)
        (put-text-property first-marker second-marker 'font-lock-face '(:foreground "red"))
        (put-text-property first-marker second-marker 'read-only t)
        )))



(ert-deftest emacs-transclusion-test/mark-code-insertion ()
  "Text markers should surround transcluded text"

  (let* ((filepath-to-transclude (make-temp-file "emacs-transclusion-test"))
        (embed-string (concat "[EMBED: " filepath-to-transclude "]")))
        

    ;Create file with contents to transclude
    (with-temp-buffer
      (insert "A")
      (write-file filepath-to-transclude))
    

    ;Create buffer to test file inclusion 
    (with-temp-buffer
      (insert embed-string)
      (beginning-of-buffer)
      (emacs-transclusion/transclude-data-for-current-buffer)
      (should (string= (buffer-string) (concat "[EMBED: " filepath-to-transclude "]A")))
      (should (= (+ 1 (length embed-string)) (marker-position first-marker) ))
      (should (= (+ 2 (length embed-string)) (marker-position second-marker))))))


(ert-deftest emacs-transclusion-test/applies-text-face-on-transclusion-data ()
  (let* ((filepath-to-transclude (make-temp-file "emacs-transclusion-test"))
        (embed-string (concat "[EMBED: " filepath-to-transclude "]")))
        

    ;Create file with contents to transclude
    (with-temp-buffer
      (insert "A")
      (write-file filepath-to-transclude))
    

    ;Create buffer to test file inclusion 
    (with-temp-buffer
      (insert embed-string)
      (beginning-of-buffer)
      (emacs-transclusion/transclude-data-for-current-buffer)
      (should (equal '(:foreground "red") (get-text-property (- (point-max) 1) 'font-lock-face)))
      (should (equal t (get-text-property (- (point-max) 1) 'read-only)))

      )))
    
;(add-hook 'before-save-hook 'emacs-transclusion/before-save-hook)
(ert-run-tests-interactively t)
