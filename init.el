;; turn on debugging
(setq debug-on-error t)
(setq debug-on-quit t)

(setq package-enable-at-startup nil)

(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

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

;; enable automatically pair braces and quotes
(electric-pair-mode 1)

;; disable electric indentation mode
(electric-indent-mode 0)

;; enable line numbers
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

;; initial window
(setq initial-frame-alist
      '(
        (width . 102) ; character
        (height . 38) ; lines
        ))


;; colorize the compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; theme
(setq custom-safe-themes t)
(load-theme 'sanityinc-tomorrow-night)


;;; dired
(add-hook 'dired-load-hook '(lambda () (require 'dired-x)))

(setq dired-omit-mode t)
(setq dired-omit-files "\\.pdf$\\|\\.pyc$\\|\\.tern-port$\\|\\node_modules$\\|\\vendor$\\|\\.mysql$\\|\\.git$\\|\\tmp$\\|__pycache__")

(dolist (p '(use-package auctex))
  (when (not (package-installed-p p))
    (package-install p)))

;;; use-package
(require 'use-package)

;; markdown
(use-package markdown-mode
  :ensure t)

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

;;; ivy-mode
(use-package swiper
  :ensure t
  :init
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t))
  :bind (("C-b" . ivy-switch-buffer)))


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


(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode))

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


;; windmove
(global-set-key (kbd "C-c <C-left>")  'windmove-left)
(global-set-key (kbd "C-c <C-right>") 'windmove-right)
(global-set-key (kbd "C-c <C-up>")    'windmove-up)
(global-set-key (kbd "C-c <C-down>")  'windmove-down)

;; ace-jumb-mode
(use-package ace-jump-mode
  :ensure t
  :bind ("C-." . ace-jump-mode))

;; exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; abbrev
(define-abbrev-table
  'global-abbrev-table
  '(("alpha" "α")
    ("beta" "β")
    ("lambda" "λ")
    ("x8" "×")
    ("gamma" "γ")
    ("theta" "θ")
    ("inf" "∞")
    ("ar1" "→")
    ("ar2" "⇒")))
(abbrev-mode 0)

;; imenu-list
(use-package imenu-list
  :ensure t
  :config
  (global-set-key (kbd "C-'") #'imenu-list-smart-toggle))

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

;; go completion
(defun setup-go ()
  (progn
    ;; automatic code formatting with gofmt
    (add-hook 'before-save-hook 'gofmt-before-save)))

(use-package go-mode
  :ensure t
  :config (setup-go))

(use-package company-go
  :ensure t
  :config
  (progn
    (add-hook
     'go-mode-hook
     (lambda ()
       (set (make-local-variable 'company-backends) '(company-go))
       (company-mode)))))

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
    (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.blade.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tpl.php\\'" . web-mode))))

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
              (setq ispell-dictionary "francais")
              (setq TeX-PDF-mode t))))

(defalias  'fsb 'flyspell-buffer)
(defalias  'fsm 'flyspell-mode)

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
    (define-key yas-minor-mode-map (kbd "<tab>") nil)
    (define-key yas-minor-mode-map (kbd "TAB") nil)
    (define-key yas-minor-mode-map (kbd "C-e C-e") 'yas-expand)
    (yas-global-mode 1)))

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
(global-set-key (kbd "C-,") 'shell-command)

;;; alias
(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'erb 'erase-buffer)
(defalias 'fc 'flycheck-mode)
(defalias 'wc 'whitespace-cleanup)

;;
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; custom-set-variables
(setq custom-file "~/.emacs.d/.custom.el")
(load custom-file)

;;; trun of debugging
(setq debug-on-error nil)
(setq debug-on-quit nil)
(setq ring-bell-function 'ignore)

;; set font size
(set-face-attribute 'default nil :family "FiraCode")
