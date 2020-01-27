;; -*- coding: utf-8; lexical-binding: t -*-
;; Contributors: Alpha Papa https://gist.github.com/alphapapa
;;               Benson Chu github.com/pestctrl

(setq emacs-transclusion/overlays '())

(setq emacs-transclusion/refernece-display-state nil)
(setq emacs-transclusion/embed-syntax-regex "\\[EMBED: \\([^\\]*?\\)\\(]\\)")

(defun emacs-transclusion/get-file-contents (path)
  "Returns content of file as string"
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

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

  (save-excursion
    (save-match-data

      ;;Goto to beginninf of the buffer and search for embedded syntax references
      (goto-char (point-min))
      (while (re-search-forward emacs-transclusion/embed-syntax-regex nil t)
        
        (when (match-string 0)

          ;;Insert text with identifiable overlay
          ;;TODO Modify text as readonly
          (emacs-transclusion/insert-overlaid
           (emacs-transclusion/get-transclusion-contents (match-string 0))
           :transcluded-content t
           'face '(:foreground "red"))
          )))))


(defun emacs-transclusion/get-transclusion-contents (embed-string)
  (let* ((embed-syntax-list (rest (append (read embed-string) nil)))
         (file-path (symbol-name (car (last embed-syntax-list))))
         (options (butlast embed-syntax-list)))
    (cond ((member :org-header-name options) (emacs-transclusion/org-header-transclusion options file-path ))
          (t (emacs-transclusion/get-file-contents file-path)))))


(defun emacs-transclusion/org-header-transclusion (options file-path)
  ;This fails...why
  (message "%s" options)
  (message "options: %s" (plist-get options :org-header-name))

  (let* ((org-header-name (plist-get options :org-header-name))
         (lines (org-export--inclusion-absolute-lines file-path org-header-name t nil)))
    (org-export--prepare-file-contents  file-path lines)))



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



(ert-deftest emacs-transclusion-test/verify-single-transclusion-text  ()
  "Verifies text is transcluded when referenced. Implicitly tests cursor position does not have to be at the beginning of the buffer."

  (let* ((filepath-to-transclude (make-temp-file "emacs-transclusion-test"))
        (embed-string (concat "[EMBED: " filepath-to-transclude "]")))

    ;Create file with contents to transclude
    (with-temp-buffer
      (insert "A")
      (write-file filepath-to-transclude))
    

    ;Create buffer to test file inclusion 
    (with-temp-buffer
      (insert embed-string)
      (emacs-transclusion/transclude-data-for-current-buffer)
      (let ((found-overlay (first (overlays-in (point-min) (point-max)))))
        (should (string= (buffer-string) (concat "[EMBED: " filepath-to-transclude "]A")))))))



(ert-deftest emacs-transclusion-test/verify-org-header-transclusion   ()
  "Test transcluion of org header"

  (let* ((filepath-to-transclude (make-temp-file "emacs-transclusion-test"))
        (embed-string (concat "[EMBED: :org-header-name \"* Header 1\" " filepath-to-transclude "]")))

    ;Create file with contents to transclude
    (with-temp-buffer
      (insert "* Header 1\nI_SHOULD_BE_TRANSCLUDED\n* Header 2\nI_SHOULD_NOT_BE_TRANSCLUDED")
      (write-file filepath-to-transclude))
    

    ;Create buffer to test file inclusion 
    (with-temp-buffer
      (insert embed-string)
      (emacs-transclusion/transclude-data-for-current-buffer)
      (let ((found-overlay (first (overlays-in (point-min) (point-max)))))
        (should (string= (buffer-string) (concat embed-string "I_SHOULD_BE_TRANSCLUDED\n")))))))


(ert-deftest emacs-transclusion-test/verify-multiple-transclusion-text  ()
  "Multiple references should be transcluded"

  (let* ((filepath-to-transclude (make-temp-file "emacs-transclusion-test"))
         (embed-string (concat "[EMBED: " filepath-to-transclude "]"
                               "[EMBED: " filepath-to-transclude "]")))

    ;Create file with contents to transclude
    (with-temp-buffer
      (insert "A")
      (write-file filepath-to-transclude))
    

    ;Create buffer to test file inclusion 
    (with-temp-buffer
      (insert embed-string)
      (emacs-transclusion/transclude-data-for-current-buffer)
      (let ((found-overlay (first (overlays-in (point-min) (point-max)))))
        (should (string= (buffer-string) (concat "[EMBED: " filepath-to-transclude "]A"
                                                 "[EMBED: " filepath-to-transclude "]A")))))))




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
      (emacs-transclusion/transclude-data-for-current-buffer)
      (should (equal '(:foreground "red") (overlay-get
                                           (first (overlays-in (point-min) (point-max)))
                                           'face)))
      ;(should (equal t (get-text-property (- (point-max) 1) 'read-only)))

      )))

(ert-deftest emacs-transclusion-test/remove-transcluded-text-on-save ()
  (let* ((filepath-to-transclude (make-temp-file "emacs-transclusion-test"))
         (filepath-to-write-buffer-to (make-temp-file "emacs-transclusiuon-test-save-buffer"))
        (embed-string (concat "HELLO [EMBED: " filepath-to-transclude "]WORLD")))
        

    ;Create file with contents to transclude
    (with-temp-buffer
      (insert "I_SHOULD_BE_REMOVED_ON_SAVE")
      (write-file filepath-to-transclude))

    ;Create buffer, transclude text, save buffer
    (with-temp-buffer
      (emacs-transclusion-mode t)
      (insert embed-string)
      (emacs-transclusion/transclude-data-for-current-buffer)
      (write-file filepath-to-write-buffer-to))

    ;Read the saved file in buffer, verify content's does not contain transcluded
    ;text
    (with-temp-buffer
      (insert-file-contents filepath-to-write-buffer-to)
      (should (equal embed-string (buffer-string))))))

    
(defun emacs-transclusion/delete-transcluded-overlays-and-text ()
  (dolist (cur-overlay (overlays-in (point-min) (point-max)))
    (when (overlay-get cur-overlay :transcluded-content)
      (delete-region (overlay-start cur-overlay)  (overlay-end cur-overlay))
      (delete-overlay cur-overlay))))




(define-minor-mode emacs-transclusion-mode
  "Toggle Emacs Transclusion Mode"
  :init-value nil
  :lighter "Emacs-Transclusion"
  (cond
   ;; When minor-mode is activated
   (emacs-transclusion-mode
    ;; Add whatever hook you want to add
    (add-hook 'before-save-hook 'emacs-transclusion/delete-transcluded-overlays-and-text ))
   ;; When minor-mode is deactivate
   (t
    ;; Remove your hooks
    (remove-hook 'before-save-hook 'emacs-transclusion/delete-transcluded-overlays-and-text))))


;(add-hook 'before-save-hook 'emacs-transclusion/before-save-hook )
(ert-run-tests-interactively t)
