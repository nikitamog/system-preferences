

;; personal preferences here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq user-full-name "Nikita Mogilevsky"
      user-email-address "nikitamog@gmail.com")

;; show this file first.
(setq initial-buffer-choice
      "~/.emacs")

;; show a wider breadth of characters.
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; consider auto-fill-mode (inserts characters)
;; or Visual-line-mode (for simple display effects)
;; https://www.emacswiki.org/emacs/LineWrap

;; Pair parentheses and other things
(electric-pair-mode 1)

;; show column numbers
(setq column-number-mode 1)

;; use visual cues instead of system sounds.
(setq visible-bell t)

;; show the time
(display-time-mode 1)

;; show battery life
(display-battery-mode 1)

;; change tabs to spaces by default
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; case-insensitive minibuffer completion
;; (setq completion-ignore-case  t)

;; mode-hooks
(add-hook 'c++-mode
          (lambda ()
            (setq-local left-margin 4)
            (set-fill-column 80)))

(add-hook 'org-mode
          (lambda ()
            (set-fill-column 60)))


;; paragraph wrapping.
(add-hook 'org-mode-hook 'auto-fill-mode)


;; go fullscreen
;;(toggle-frame-fullscreen)

;; set font
;; https://input.fontbureau.com/
;; saved in ~/.local/share/fonts
(add-to-list 'default-frame-alist '(font . "Input Mono Light"))
;;(add-to-list 'default-frame-alist '(font . "Input Serif Light"))

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
;; some helpful stuff from
;; https://rodogi.github.io/emacs-config-file/
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; The following is to fix a bug to be able to expand
;; '<s' to code block.
(when (version<= "9.2" (org-version))
		(require 'org-tempo))

(setq org-directory "~/Documents/notes")
(setq org-agenda-files (list "~/Documents/notes/task_reserve.org"
                             "~/Documents/notes/in_progress.org"))
(setq org-default-notes-file (concat org-directory "/reference.org"))

(add-to-list 'load-path (concat user-emacs-directory "org-bullets"))

(require 'org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)

(setq org-ellipsis "⤵")
(setq org-src-tab-acts-natively t)

;; Close TODOs with a timestamp
(setq org-log-done 'time)

;; global defaults for org
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c C-o") 'org-open-at-point-global)
(global-set-key (kbd "C-c C-l") 'org-insert-link-global)
(global-set-key (kbd "C-c o") 'org-info)

(setq org-todo-keywords
      '((sequence "TODO" "IN PROGRESS" "DONE")))
(setq org-enforce-todo-dependencies t)

(setq org-tag-alist '(("@work" . ?w) ("@home" . ?h) ("laptop" . ?l)))

;; resolve idle time when away from the computer.
(setq org-clock-idle-time 15)

;; org-agenda stuff
(setq org-agenda-include-diary t)

;; margins
;;(setq left-margin-width 3)

;; margins alternate
(fringe-mode '(16 . 16))

;; cc modes and the like.
(setq-default c-default-style "linux"
              c-basic-offset 4)

;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

;; hotkey for compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

(setq dired-listing-switches "-AlShr")

;; line breaks
;; (dolist (hook '(python-mode-hook prog-mode-hook list-mode-hook))
;;   (add-hook hook (lambda () (set-fill-column 80))))

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

;; (add-to-list 'load-path (concat user-emacs-directory "xterm-color"))
;; (add-to-list 'load-path (concat user-emacs-directory "eterm-256color"))

;; (require 'eterm-256color)

;; company (complete any) front-end completion framework
;; https://github.com/company-mode/company-mode
(add-to-list 'load-path (concat user-emacs-directory "company-mode"))

;; irony back-end completion framework
;; https://github.com/Sarcasm/irony-mode
(add-to-list 'load-path (concat user-emacs-directory "irony-mode"))

;; the package to bridge the two
;; https://github.com/Sarcasm/irony-mode
(add-to-list 'load-path (concat user-emacs-directory "company-irony"))

;; an outline mode for file navigation
;; https://github.com/Alexander-Miller/treemacs
(add-to-list 'load-path (concat user-emacs-directory "treemacs"))

;; ggtags for code navigation
;; https://github.com/leoliu/ggtags

;; flycheck for linting
;; https://github.com/flycheck/flycheck
(add-to-list 'load-path (concat user-emacs-directory "flycheck"))

;; flycheck-irony to integrate the backend
;; https://github.com/Sarcasm/flycheck-irony
(add-to-list 'load-path (concat user-emacs-directory "flycheck-irony"))

;; conveniences for matching symbols.
;; https://github.com/Fuco1/smartparens

;; highlights changes that have not been commited.
;; https://github.com/dgutov/diff-hl

;; solarized theme for easy eyes.
;; https://github.com/bbatsov/solarized-emacs
(add-to-list 'load-path (concat user-emacs-directory "solarized-emacs"))

;; popwin to deal with those pesky window popups
;; https://github.com/m2ym/popwin-el
(add-to-list 'load-path (concat user-emacs-directory "popwin-el"))
(require 'popwin)
(popwin-mode 1)

;; load them in!
(require 'company)
(require 'irony)
(require 'company-irony)
(require 'solarized)
(load-theme 'solarized-dark t)
(require 'flycheck)
(require 'flycheck-irony)
;; company mode in all buffers
(add-hook 'after-init-hook 'global-company-mode)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
  
;; lean-mode
;;(add-to-list 'load-path (concat user-emacs-directory "lean-mode"))
;;(require 'lean-mode)

;; CODE FOLDING
;;    (defun sg-toggle-fold ()
;;    "Toggle code folding according to indentation of current line."
;;    (interactive)
;;    (set-selective-display
;;    (if selective-display
;;    nil
;;    (save-excursion
;;    (back-to-indentation)
;;    (1+ (current-column))))))


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
 '(org-agenda-files
   (quote
    ("~/Documents/notes/journal.org" "~/Documents/notes/task_reserve.org" "~/Documents/notes/in_progress.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
