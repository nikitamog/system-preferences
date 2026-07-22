;;; -*- lexical-binding: t -*-

;; ==================================================
;; dumps

(defun nm/get-delimited-prefix (raw-string delimiter)
  (let ((regex (format "%s\\(.*?\\)%s" delimiter delimiter)))
    (if (string-match regex raw-string)
        (match-string 1 raw-string)
      nil)))

(defun nm/build-order (literate-config-path)
  "Gets list of files sorted by first appearance within literate config."
  (with-current-buffer (find-file-noselect literate-config-path)
    (unless (derived-mode-p 'org-mode)
      (user-error "Not in an org-mode buffer"))
    (let (ordered-list)
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (hl)
          (let ((header-title
                 (string-trim
                  (org-element-interpret-data (org-element-property :title hl)))))
            (push (format "%s.el" (nm/get-delimited-prefix header-title "=")) ordered-list))))
      (seq-uniq (nreverse ordered-list)))))

(defun nm/dump-emacs-build-sequence ()
  (interactive)
  (let* ((custom-dir           "~/code/system-preferences")
	 (target-path          (expand-file-name "nm-emacs-build-sequence.txt" custom-dir))
	 (literate-config-path (expand-file-name "user-config.org" custom-dir)))
    (unless (file-exists-p literate-config-path)
      (error (message "DNE for %s: %s" file (error-message-string err))))
    (with-temp-buffer
      (insert (string-join (nm/build-order literate-config-path) "\n"))
      (write-region (point-min) (point-max) target-path)
      (message "Done! Build sequence written to %s" target-path))))

;; ==================================================
;; loads

(defun nm/reload-emacs-config ()
  (interactive)
  (nm/dump-emacs-build-sequence)
  (let* ((custom-dir "~/code/system-preferences")
         (literate-config-path (expand-file-name "user-config.org" custom-dir)))
  (with-current-buffer (find-file-noselect literate-config-path)
    (org-babel-tangle))
  (nm/load-nm-files "~/code/system-preferences")))

(defun nm/load-emacs-build-sequence ()
  ;; concept copied from yuanw's Everybody Codes
  (let* ((custom-dir     "~/code/system-preferences")
	 (directory      (expand-file-name "nm-emacs-build-sequence.txt" custom-dir))
         (sequence-stack '()))
    (with-temp-buffer
      (insert-file-contents directory)
      (goto-char (point-min))
      (while (not (eobp))
        (push (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))
              sequence-stack)
        (forward-line 1))
      (nreverse sequence-stack))))

(defun nm/load-nm-files (directory)
  (let* ((expanded-dir (expand-file-name directory))
         (files          (nm/load-emacs-build-sequence)))
    (dolist (file files)
      (let ((path (expand-file-name file expanded-dir)))
        (if (file-regular-p path)
            (progn
              (message "Executing: %s" path)
              (condition-case err
                  (load path nil t)
                (error
                 (message "Error in %s: %s" file (error-message-string err)))))
          (message "Skipping: %s (not a regular file)" path))))))

(provide 'nm-sys-utils)
