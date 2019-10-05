

;; put customize data here
;;(setq custom-file (concat user-emacs-directory "custom.el"))
;;(when (file-exists-p custom-file)
;;  (load custom-file))

;; personal preferences here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use visual cues instead of system sounds.
(setq visible-bell t)

;; show the time
(display-time-mode 1)

;; change tabs to spaces by default
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; go fullscreen
;;(toggle-frame-fullscreen)

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

;;irc preferences
(setq rcirc-default-nick "nikitamog")
;;(add-to-list 'rcirc-server-alist
;;             '("irc.freenode.net"
;;               :channels ("#emacs")))


;; backup files
(setq backup-directory-alist `(("." . "~/.saves")))


;; org-mode
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; margins
(setq left-margin-width 3)

;; Cedet setup
(load-file (concat user-emacs-directory "/cedet/cedet-devel-load.el"))
(load-file (concat user-emacs-directory "cedet/contrib/cedet-contrib-load.el"))

;; cc modes and the like.
(setq-default c-default-style "linux"
              c-basic-offset 4)

;; END PERSONAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PACKAGE REQUIRES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; modern file api
(add-to-list 'load-path (concat user-emacs-directory "f.el"))

;; modern string library
(add-to-list 'load-path (concat user-emacs-directory "s.el"))

;; modern list library
(add-to-list 'load-path (concat user-emacs-directory "dash.el"))

(add-to-list 'load-path (concat user-emacs-directory "xterm-color"))
(add-to-list 'load-path (concat user-emacs-directory "eterm-256color"))

(require 'eterm-256color)

;; (eval-after-load 'company
;;      '(add-to-list 'company-backends 'company-irony)

;;irony
;;company

;; END PACKAGE REQUIRES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)


(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes
   (quote
    ("d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(inhibit-startup-screen nil)
 '(initial-buffer-choice "~/.emacs")
 '(package-selected-packages
   (quote
    (company-irony-c-headers company-irony company-c-headers company solarized-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
