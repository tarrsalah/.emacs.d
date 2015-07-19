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

;;; misc settings
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode 1)
(delete-selection-mode 1)
(global-hl-line-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

(setq initial-scratch-message ""
      inhibit-startup-message t)

;; from http://www.emacswiki.org/emacs/BackupDirectory#toc1
(setq
   backup-by-copying t
   backup-directory-alist
    '(("." . "~/.saves"))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)

;; enable erace buffer
(put 'erase-buffer 'disabled nil)

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

;;; expand region
(use-package expand-region
	:ensure t
	:bind (("C-=" . er/expand-region)))

;; company
(use-package company
	:ensure t
	:init
	(add-hook 'after-init-hook 'global-company-mode)
	:config
	(progn
		(setq company-tooltip-limit 10)
		(setq company-idle-delay .1)
		(setq company-echo-delay 0)
		(setq company-begin-commands '(self-insert-command)))
	:bind (("C-n" . company-complete)))

;; grizzl for projectile completion
(use-package grizzl
	:ensure t
	:config
	(custom-set-faces
	 '(grizzl-selection-face ((t (:foreground "#8F9D6A"))))))

;; projectile
(use-package projectile
	:ensure t
	:config
	(progn
		(projectile-global-mode)
		(setq projectile-completion-system 'grizzl)
		(setq projectile-enable-caching t)
		(add-to-list
		 'projectile-globally-ignored-directories "node_modules")))

;; flycheck
(use-package flycheck
  :ensure t)

;; org-mode
(use-package toc-org
  :ensure t
  :config
  (add-hook 'org-mode-hook 'toc-org-enable))

;; javascript
(use-package js2-mode
	:ensure t
	:init
	(progn
		(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
		(add-to-list 'auto-mode-alist '("\\.jsx$" . js2-mode)))
	:config
	(progn
	  (add-hook 'js2-mode-hook
		    (lambda()
		      (progn
			(tern-mode t)
			(setq js2-basic-offset 2)
			(flycheck-mode))))
	  (setq-default js2-additional-externs '("require" "module"))
	  (use-package company-tern
	    :ensure t
	    :config
	    (add-to-list 'company-backends 'company-tern))
	  (flycheck-add-mode 'javascript-eslint 'js2-mode)
	  (setq-default flycheck-disabled-checkers
			(append flycheck-disabled-checkers
				'(javascript-jshint)))
	  (setq-default flycheck-disabled-checkers
			(append flycheck-disabled-checkers
				'(json-jsonlist)))))

;;; key bindings
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "<escape>") (kbd "C-g"))
(global-set-key (kbd "C-<menu>") 'menu-bar-open)
(global-set-key (kbd "C-x e") 'erase-buffer)

;;; alias
(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'erb 'erase-buffer)
(defalias 'fc 'flycheck-mode)
(defalias 'wc 'whitespace-cleanup)

;;; trun of debugging
(setq debug-on-error nil)
(setq debug-on-quit nil)
;;; init.el ends here
