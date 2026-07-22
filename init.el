;;; -*- lexical-binding: t -*-

;; ==================================================
;; straight init

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; ==================================================
;; user init

(let* ((user-elisp-dir (expand-file-name "~/code/elisp")))
  (unless (and (file-exists-p user-elisp-dir)
	       (file-directory-p user-elisp-dir))
    (error
     (message "Error in %s: %s" user-elisp-dir (error-message-string err))))
  (add-to-list 'load-path user-elisp-dir))

(require 'nm-sys-utils)

(nm/load-nm-files "~/code/system-preferences")

;;;; old literate config approach
;; (org-babel-load-file
;;  (expand-file-name "user-config.org"
;;                    user-emacs-directory))

;; ==================================================
;; auto emacs

(put 'narrow-to-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ignored-local-variable-values
   '((org-export-with-title . t) (org-export-with-properties)
     (eval require 'org-make-toc))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'list-timers 'disabled nil)
