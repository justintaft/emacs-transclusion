;; Contributor: Alpha Papa https://gist.github.com/alphapapa

(setq emacs-transclusion/overlays '())

(setq emacs-transclusion/refernece-display-state nil)
(setq emacs-transclusion/embed-syntax-regex "\\[EMBED: \\([^\\]*?\\)\\(]\\)")

(defun emacs-transclusion/get-file-contents (path)
  "Returns content of file as string"
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(define-minor-mode emacs-transclusion-mode
  "Toggle Emacs Transclusion Mode"
  :init-value nil
  :ligher "Emacs-Transclusion")
  
;(defun emacs-transclusion/toggle-display-embed-syntax ()
;  (interactive)
;
;  "Toggles displaying of embed syntax in documents."
;
;  ;;Inverse turthyness of reference display state
;  (setq emacs-transclusion/refernece-display-state (not emacs-transclusion/refernece-display-state))
;  (dolist (cur-overlay emacs-transclusion/overlays)
;    (overlay-put cur-overlay 'invisible emacs-transclusion/refernece-display-state)))

(defun emacs-transclusion/insert-overlaid (string &rest properties)
  "Insert STRING and overlay it, then return the overlay.
If PROPERTIES, add them as properties to the overlay."
  (let* ((beg (point))
         (end (progn
                (insert string)
                (point)))
         (ov (make-overlay beg end)))
    (when properties
      (cl-loop for (key value) on properties by #'cddr
               do (overlay-put ov key value)))
    ov))

(defun emacs-transclusion/transclude-data-for-current-buffer ()
  "Finds embed references in buffer, and transcludes data into buffer."

  (interactive)
  (let ((buffer-point (re-search-forward emacs-transclusion/embed-syntax-regex)))
        
      (when (match-string 0)
	(setq overlay-ref (make-overlay (match-beginning 0) (match-end 0)))
	(push overlay-ref emacs-transclusion/overlays)


        ;;Insert text with identifiable overlay
        ;;TODO Modify text as readonly
        (emacs-transclusion/insert-overlaid
         (emacs-transclusion/get-file-contents (match-string 1))
         :transcluded-content t
         'face '(:foreground "red"))

        ;(put-text-property first-marker second-marker 'font-lock-face '(:foreground "red"))
        ;(put-text-property first-marker second-marker 'read-only t)

        )))



(ert-deftest emacs-transclusion-test/overlay-range ()
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
      (let ((found-overlay (first (overlays-in (point-min) (point-max)))))
        (should (= (+ 1 (length embed-string)) (overlay-start found-overlay) ))
        (should (= (+ 2 (length embed-string)) (overlay-end found-overlay)))))))



(ert-deftest emacs-transclusion-test/verify-single-etransclusion-text  ()
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
      (let ((found-overlay (first (overlays-in (point-min) (point-max)))))
        (should (string= (buffer-string) (concat "[EMBED: " filepath-to-transclude "]A")))))))



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
      (should (equal '(:foreground "red") (overlay-get
                                           (first (overlays-in (point-min) (point-max)))
                                           'face)))
      ;(should (equal t (get-text-property (- (point-max) 1) 'read-only)))

      )))

(ert-deftest emacs-transclusion-test/remove-transcluded-text-on-save ()
  (let* ((filepath-to-transclude (make-temp-file "emacs-transclusion-test"))
         (filepath-to-write-buffer-to (make-temp-file "emacs-transclusion-test-save-buffer"))
        (embed-string (concat "HELLO [EMBED: " filepath-to-transclude "]WORLD")))
        

    ;Create file with contents to transclude
    (with-temp-buffer
      (write-file filepath-to-transclude))

    ;Create buffer, transclude text, save buffer
    (with-temp-buffer
      (insert embed-string)
      (beginning-of-buffer)
      (emacs-transclusion/transclude-data-for-current-buffer)
      (write-file filepath-to-write-buffer-to))

    ;Read the saved file in buffer, verify content's does not contain transcluded
    ;text
    (with-temp-buffer
      (insert-file-contents filepath-to-write-buffer-to)
      (should (equal embed-string (buffer-string))))))
    
;(add-hook 'before-save-hook 'emacs-transclusion/before-save-hook)
(ert-run-tests-interactively t)
