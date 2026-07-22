#!/bin/env bash

tangle-from-here-emacs-user-config() {
    local target_dir="~/code/system-preferences/user-config.org"
    emacs --batch \
	  --eval '(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))' \
	  -l org \
	  --eval "(org-babel-tangle-file \"$target_dir\")"
}

dump-emacs-user-build-sequence() {
    emacs --batch \
	  -l ~/code/elisp/nm-sys-utils.el \
	  -l org \
	  --eval "(nm/dump-emacs-build-sequence)"
}

tangle-from-here-emacs-user-config
dump-emacs-user-build-sequence
