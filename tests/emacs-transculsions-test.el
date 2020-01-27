(require 'ert)
(require 'emacs-transclusion)

(ert-deftest emacs-transclusion-test/remove-transcluded-text-on-save ()
  (let* ((filepath-to-transclude (make-temp-file "emacs-transclusion-test"))
         (filepath-to-write-buffer-to (make-temp-file "emacs-transclusion-test-save-buffer"))
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
                                           'face))))))
