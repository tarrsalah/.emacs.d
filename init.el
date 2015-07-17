;;; turn on debugging
(setq debug-on-error t)
(setq debug-on-quit t)

(setq package-enable-at-startup nil)
(require 'package)

(add-to-list 'package-archives
	     '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
	     '("elpa" . "http://tromey.com/elpa/"))

(package-initialize)

;;; install first-class packages
(defvar my-packages
  '(use-package))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;; use-package
(require 'use-package)

;;; twilight color theme
(use-package color-theme
  :ensure t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-file "~/.emacs.d/themes/color-theme-twilight.el")
(color-theme-twilight)

;; markdown
(use-package markdown-mode
  :ensure t)

;; magit
(use-package magit
  :ensure t
  :bind (("C-x g". magit-status)))

;;; org-mode
(setq org-support-shift-select t)

;; ido
(setq my/ido-order '(".tex" ".go" ".clj" ".el" ".lisp" ".cl" ".ini" ".cfg" ".cnf"))
(use-package ido
  :init (ido-mode 1)
  :config
  (progn
    (setq ido-enable-flex-matching t
	  ido-everywhere t
	  ido-file-extensions-order my/ido-order))
  :bind (("C-x C-b" . ibuffer)
	 ("C-b" . switch-to-buffer)))

;;; alias
(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'erb 'erase-buffer)

(defalias 'wc 'whitespace-cleanup)

;;; trun of debugging
(setq debug-on-error nil)
(setq debug-on-quit nil)
