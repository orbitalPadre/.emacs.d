;;; init.el --- -*- lexical-binding: t -*-

;; tune the gc (garbage collection)
;; default emacs gc settings are too conservative for modern machines
;; making emacs spend too much time collecting garbage in alloc-heavy code
(setq gc-cons-threshold (* 4 1024 1024))
(setq gc-cons-percentage 0.3)

;; set custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(setq user-full-name "first last"
      user-mail-address "orbitalpadre@gmail.com")

;; always load newest bytecode
(setq load-prefer-newer t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; newline at end of files
(setq require-final-newline t)

;; wrap lines at 80 characters
(setq-default fill-columm 80)

;; delete selection with a keypress
(delete-selection-mode t)

;; disable gui stuff
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(if (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))

(blink-cursor-mode -1)

(setq use-file-dialog nil)
(setq use-dialog-box nil)

;; start emacs with empty scratch buffer
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")

;; set the font
(set-face-attribute 'default nil :family "JetBrains Mono" :height 100)

;; disable lock-files and backup files
(setq create-lockfiles nil)
(setq make-backup-files nil)

;; move auto-save files to saner location
(let ((auto-save-dir (file-name-as-directory (expand-file-name "autosave" user-emacs-directory))))
  (setq auto-save-list-file-prefix (expand-file-name ".saves-" auto-save-dir))
  (setq auto-save-file-name-transforms (list (list ".*" (replace-quote auto-save-dir) t))))

;; default to UTF-8
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)

;; fix scrolling
(setq mouse-wheel-progressive-speed nil)
(setq scroll-margin 3)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 'always)

;; set undo limits
(setq undo-limit (* 16 1024 1024))
(setq undo-strong-limit (* 24 1024 1024))
(setq undo-outer-limit (* 64 1024 1024))


;; do not disable commands
(setq disabled-command-function nil)

;; disable vc (version control)
(setq vc-handled-backends '())

;;; end of init.el
