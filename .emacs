;;; Custom Emacs Settings
;;; Nikita Mogilevsky
;;; nikitamog@gmail.com
;;;

;; Should I use common lisp?
;; (require 'cl)

;; Define emacs-root if necessary

;; add all the elisp directories under ~/emacs
;; into the load path.
;; (labels ((add-path (p)
;;          (add-to-list 'load-path
;;                         (concat emacs-root p))))
;;  (add-path "emacs/")
;;  )

;; packages
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packes/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-tolist 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; Add token for paradox packages
(setq paradox-github-token "b311f6c5696d02efb4ddcf93d3b965a34c317b87")

;; Set paradox as the default emacs package browser
(require 'paradox)
(paradox-enable)

;; personal preferences here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; change tabs to spaces by default
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

;; set font
(add-to-list 'default-frame-alist '(font . "Latin Modern Mono 10 Regular"))

;;bypass using the meta key
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;;mistake convenience
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;;remove stupid menus
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; load configurations and whatnot
;; (if-not-terminal
;;  (load-library "")

;; run the shell
(shell)

;;; end .emacs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized)))
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(package-selected-packages
   (quote
    (solarized-theme paradox color-theme-solarized better-shell python chess))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
