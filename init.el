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
  :ensure t)
