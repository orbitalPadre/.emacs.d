;;; init.el -- emacs configuration file -*- lexical-binding: t-*-

;; garbage collection tuning
(setq gc-cons-threshold (* 4 1024 1024))
(setq gc-cons-percentage 0.3)

;; startup info
(let ((emacs-start-time (current-time)))
  (add-hook 'emacs-startup-hook
	    (lambda ()
	      (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
		(message "[emacs initialized in %.3fs]" elapsed)))))

;; custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; prepare package archives
;; melpa-stable needed for markdown-mode
(require 'package)
(add-to-list 'package-archives '("melpa"        . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org"          . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("gnu"          . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(use-package try :ensure t)
(use-package which-key :ensure t :config (which-key-mode))

(unless (package-installed-p 'markdown-mode)
  (package-refresh-contents)
  (package-install 'markdown-mode))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'"       . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; download & enable magit
(unless (package-installed-p 'magit)
  (package-refresh-contents)
  (package-install 'magit))

(use-package magit
  :ensure t
  :init
  (progn (bind-key "C-x g" 'magit-status)))

(require 'magit)

;; download & enable evil
(unless (package-installed-p 'evil)
  (package-refresh-contents)
  (package-install 'evil))
(require 'evil)
(setq evil-undo-system 'undo-fu)
(evil-mode 1)

;; download & enable undo-fu
(unless (package-installed-p 'undo-fu)
  (package-refresh-contents)
  (package-install 'undo-fu))
(require 'undo-fu)

;; download & enable yasnippet
(unless (package-installed-p 'yasnippet)
  (package-refresh-contents)
  (package-install 'yasnippet))
(require 'yasnippet)
(yas-global-mode 1)

(unless (package-installed-p 'auto-complete)
  (package-refresh-contents)
  (package-install 'auto-complete))
(require 'auto-complete)

(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)))

(unless (package-installed-p 'flycheck)
  (package-refresh-contents)
  (package-install 'flycheck))
(require 'flycheck)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(unless (package-installed-p 'modern-cpp-font-lock)
  (package-refresh-contents)
  (package-install 'modern-cpp-font-lock))
(require 'modern-cpp-font-lock)

(use-package modern-cpp-font-lock
  :ensure t)

;; compilation for cpp code
(defun code-compile ()
  (interactive)
  (unless (file-exists-p "Makefile")
    (set (make-local-variable 'compile-command)
	 (let ((file (file-name-nondirectory buffer-file-name)))
	   (format "%s -o %s %s"
		   (if (equal (file-name-extension file) "cpp") "g++" "gcc")
		   (file-name-sans-extension file)
		   file)))
    (compile compile-command)))

(global-set-key [f9] 'code-compile)

;; search highlighting
(setq search-highlight t)
(setq query-replace-highlight t)

;; text-mode
(add-hook 'text-mode-hook #'visual-line-mode)

;; gui tuning
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; font?
(set-face-attribute 'default nil :font "JetBrains Mono 11")

;; line-numbers
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

;; move cursor where it was on last visit
(save-place-mode t)

;; show parenthesis
(show-paren-mode 1)
(setq show-paren-delay 0)



;; disable lock files, backup files and move autosave files to better location
(setq create-lockfiles nil)
(setq make-backup-files nil)
(let ((auto-save-dir
       (file-name-as-directory (expand-file-name "autosaves" user-emacs-directory))))
  (setq auto-save-list-file-prefix (expand-file-name ".saves-" auto-save-dir))
  (setq auto-save-file-name-transforms (list (list ".*" (replace-quote auto-save-dir) t))))

;; use UTF-8
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

;; clipboard
(setq-default select-active-regions nil)
(when (boundp 'x-select-enable-primary)
  (setq x-select-enable-primary nil))

;; set undo limits
(setq undo-limit (* 16 1024 1024))
(setq undo-strong-limit (* 24 1024 1024))
(setq undo-outer-limit (* 64 1024 1024))

;; do not disable commands?
(setq disabled-command-function nil)

;; disable version control
(setq vc-handled-backends '())

;;; init.el ends here
