;;; emacs-transclusion.el --- Transclude files in Emacs ;; -*- coding: utf-8; lexical-binding: t -*-
;; Contributors: Alpha Papa https://gist.github.com/alphapapa
;;               Benson Chu github.com/pestctrl

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defvar emacs-transclusion/overlays '())
(defvar emacs-transclusion/refernece-display-state nil)
(defvar emacs-transclusion/embed-syntax-regex "\\[EMBED: \\([^\\]*?\\)\\(]\\)")

(defun emacs-transclusion/get-file-contents (path)
  "Return content of file at PATH as string."
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
  "Find embed references in buffer. Transclude data into buffer."
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
           'face '(:foreground "red")))))))

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
(defun emacs-transclusion/delete-transcluded-overlays-and-text ()
  "Remove transclusion overlays from buffer."
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
    (emacs-transclusion/transclude-data-for-current-buffer)
    (add-hook 'before-save-hook 'emacs-transclusion/delete-transcluded-overlays-and-text)
    (add-hook 'after-save-hook 'emacs-transclusion/transclude-data-for-current-buffer))
   ;; When minor-mode is deactivate
   (t
    ;; Remove your hooks
    (remove-hook 'before-save-hook 'emacs-transclusion/delete-transcluded-overlays-and-text))))

(provide 'emacs-transclusion)
;;(ert t)
;;; emacs-transclusion.el ends here
