;; turn on debugging
(setq debug-on-error t)
(setq debug-on-quit t)

(setq package-enable-at-startup nil)

(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(when (< emacs-major-version 23)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))

;;; misc settings
(tool-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode 1)
(delete-selection-mode 1)
(global-hl-line-mode 1)
(menu-bar-mode 0)
(global-auto-revert-mode)

(electric-pair-mode 1)
(electric-indent-mode 1)
(global-linum-mode t)

;; enable a key in dired
(put 'dired-find-alternate-file 'disabled nil)

(fset 'yes-or-no-p 'y-or-n-p)
(
setq initial-scratch-message ""
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

;; 4 spaces for tab
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; xml
(setq
    nxml-child-indent 4
    nxml-attribute-indent 4
    nxml-slash-auto-complete-flag t)

;; colorize the compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;; use-package
(require 'use-package)


;; theme
(setq custom-safe-themes t)
(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-night))


;; ido
(require 'ido)
(ido-mode)
(ido-everywhere)
(setq ido-create-new-buffer 'always)
(global-set-key (kbd "C-b") 'ido-switch-buffer)

;; ido flex
(use-package flx-ido
  :ensure t
  :config
  (progn
    (setq ido-enable-flex-matching t)
    (setq ido-enable-prefix t)
    (setq ido-enable-flex-matching t)))

;; ido-vertical-mode
(use-package ido-vertical-mode
  :ensure t
  :config (ido-vertical-mode))

;; magit integration
(setq magit-completing-read-function #'magit-ido-completing-read)

;; flx-ido
(require 'flx-ido)
(flx-ido-mode 1)
(setq ido-use-faces nil)

;;; dired
(add-hook 'dired-load-hook '(lambda () (require 'dired-x)))

(setq dired-omit-mode t)
(setq dired-omit-files "\\.pdf$\\|\\.pyc$\\|\\.tern-port$\\|\\tmp$\\|__pycache__")

(dolist (p '(use-package auctex))
  (when (not (package-installed-p p))
    (package-install p)))

;; markdown
(use-package markdown-mode
  :ensure t)

;; popup kill ring
(use-package popup-kill-ring
  :ensure t
  :bind (("M-y" . popup-kill-ring)))

;; magit
(use-package magit
  :ensure t
  :bind (("C-x g". magit-status)))

;;; org-mode
(setq org-support-shift-select t)
(setq org-hide-leading-stars t)

;;; ibuffer
(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))


;;; expand region
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

;; company
(use-package company
  :ensure t
  :diminish company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (progn
    (setq company-tooltip-limit 10)
    (setq company-idle-delay 1)
    (setq company-dabbrev-downcase nil)
    (setq company-echo-delay 0)
    (setq company-begin-commands '(self-insert-command)))
  :bind (("C-n" . company-complete)))

;; paredit
(use-package paredit
  :ensure t)


;; projectile
(use-package projectile
  :ensure t
  :config
  (progn
    (projectile-mode +1)
    (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (add-to-list
     'projectile-globally-ignored-directories "dist")
    (add-to-list
     'projectile-globally-ignored-directories ".db")
    (add-to-list
     'projectile-globally-ignored-directories "./.db")
    (add-to-list
     'projectile-globally-ignored-directories "vendor")
    (add-to-list
     'projectile-globally-ignored-directories "node_modules")))

;; flycheck
(use-package flycheck
  :diminish flycheck-mode
  :ensure t)

;; smex
(use-package smex
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'smex))

;; ace-window
(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-x o") 'ace-window))


(use-package highlight-indentation
  :ensure t)
(set-face-background 'highlight-indentation-current-column-face "#666")
(highlight-indentation-mode 1)

;; ace-jumb-mode
(use-package ace-jump-mode
  :ensure t
  :bind ("C-." . ace-jump-mode))

;; exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

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
    (add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx$" . js2-mode)))

  :config
  (progn
    (use-package js2-refactor
      :ensure t
      :config
      (progn
        (add-hook 'js2-mode-hook #'js2-refactor-mode)
        (js2r-add-keybindings-with-prefix "C-c C-m")))

    (add-hook 'js2-mode-hook
              (lambda()
                (progn
                  (tern-mode t)
                  (abbrev-mode t)
                  (setq js2-basic-offset 2)
                  (flycheck-mode))))

    (js2-mode-hide-warnings-and-errors)
    (setq-default js2-additional-externs '("require" "module"))
    (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

    (use-package company-tern
      :ensure t
      :config
      (add-to-list 'company-backends 'company-tern))

    (use-package xref-js2
      :ensure t
      :config
      (progn
        (define-key js2-mode-map (kbd "M-.") nil)
        (add-hook 'js2-mode-hook (lambda ()
                                   (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))))

    (use-package prettier-js
      :ensure t
      :config
      (progn
        (add-hook 'json-mode-hook 'prettier-js-mode)
        (add-hook 'css-mode-hook 'prettier-js-mode)
        (add-hook 'js2-mode-hook 'prettier-js-mode)))

    (flycheck-add-mode 'javascript-eslint 'js2-mode)
    (setq js-indent-level 2)

    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(javascript-jshint)))
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(json-jsonlist)))))

(use-package virtualenvwrapper
  :ensure
  :config
  (progn
    (setq venv-location "~/workon")
    (venv-initialize-interactive-shells)
    (venv-initialize-eshell)))

(use-package company-jedi
  :ensure t
  :config
  (progn
    (jedi:setup)
    (add-to-list 'company-backends 'company-jedi)))

;; geiser
(use-package geiser
  :ensure t)

;; golang
(use-package go-mode
  :ensure t
  :config 
  ( progn
    (add-hook 'before-save-hook 'gofmt-before-save)))

(use-package company-go
  :ensure t
  :config
  (progn
    (add-hook
     'go-mode-hook
     (lambda ()
       (set (make-local-variable 'company-backends) '(company-go))
       (company-mode)))))

(use-package go-rename
  :ensure t)

;; web-mode
(use-package web-mode
  :ensure t
  :config
  (progn
    (electric-pair-mode 1)
    (setq web-mode-markup-indent-offset 4)
    (setq web-mode-enable-auto-pairing nil)
    (setq web-mode-css-indent-offset 4)
    (setq web-mode-code-indent-offset 4)
    (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.blade.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tpl.php\\'" . web-mode))))


(use-package php-mode
  :ensure t
  :mode (("\\.php\\'" . php-mode))
  :init
  (if
      (file-exists-p "~/.config/composer/vendor/bin" )
      (setq flycheck-php-phpcs-executable "~/.config/composer/vendor/bin/phpcs"
  	        phpunit-program "~/.config/composer/vendor/bin/phpunit")
    (warn "Can't find composer bin directory, some tools might not work"))

  (setq flycheck-phpcs-standard "PSR2")
  (setq php-mode-coding-style (quote psr2))
  (setq php-style-delete-trailing-whitespace 1))

(use-package php-cs-fixer
  :ensure t)

;; jade-mode for pug files
(use-package jade-mode
  :ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.pug\\'" . jade-mode))))

;; emmet-mode
(use-package emmet-mode
  :ensure t
  :config
  (progn
    (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'js2-mode 'emmet-mode)
    (add-hook 'html-mode-hook 'emmet-mode)
    (add-hook 'css-mode 'emmet-mode)
    (add-hook 'less-css-mode 'emmet-mode)))

;; latex
(require 'tex)
(setq-default TeX-master nil)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)

(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(setq TeX-source-correlate-start-server t)


(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t)

;; drupal 7
(add-to-list 'auto-mode-alist '("\\.info\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.module\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.install\\'" . web-mode))

;; docker
(use-package dockerfile-mode
  :ensure t)

(use-package restclient
  :ensure t)

;; nginx
(use-package nginx-mode
  :ensure t)

;; apache
(use-package apache-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.conf\\'" . apache-mode)))

;; latex
(setq font-latex-fontify-sectioning 1.0)
(setq reftex-plug-into-AUCTeX t)
(setq Tex-Source-Correlate t)
(setq TeX-output-view-style
      (quote
       (("^pdf$" "." "evince -f %o")
        ("^html?$" "." "iceweasel %o"))))


(add-hook 'text-mode-hook
          (lambda ()
            (progn
              (flyspell-mode 0)
            ;;(setq ispell-dictionary "francais")
              (setq TeX-PDF-mode t))))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (TeX-fold-mode 1)))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (push
             '("Latexmk" "latexmk -bibtex -pdf %s" TeX-run-TeX nil t
               :help "Run Latexmk on file")
             TeX-command-list)))

;; yasnippet
(use-package yasnippet
  :ensure t
  :config
  (progn
    (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
    (yas-global-mode 1)))

;; dump-jump
(use-package dumb-jump
  :ensure
  :bind (("C-x j" . dumb-jump-go)
         ("M-g j" . dumb-jump-go-prompt)))

(use-package less-css-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'css-mode-hook  (lambda () (rainbow-mode 1))))

(use-package rainbow-delimiters
  :ensure t)

;;; key bindings
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "<escape>") (kbd "C-g"))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;;; trun of debugging
(setq debug-on-error nil)
(setq debug-on-quit nil)
(setq ring-bell-function 'ignore)

;; set font size
(set-face-attribute 'default nil :family "FiraCode" :height 110)
