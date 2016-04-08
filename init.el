;; turn on debugging
(setq debug-on-error t)
(setq debug-on-quit t)

(setq package-enable-at-startup nil)
(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))

(package-initialize)

;;; misc settings
(tool-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode 1)
(delete-selection-mode 1)
(global-hl-line-mode 1)

;; enable automatically pair braces and quotes
(electric-pair-mode 1)

;; disable electric indentation mode
(electric-indent-mode 0)

;; enable line numbers
(global-linum-mode t)

;; enable a key in dired
(put 'dired-find-alternate-file 'disabled nil)

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

;; 4 spaces for tab
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; colorize the compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; kill the buffer's window when killing the buffer itself
;; http://www.emacswiki.org/emacs/misc-cmds.el
(defun kill-buffer-and-its-windows (buffer)
  "Kill BUFFER and delete its windows.  Default is `current-buffer'.
BUFFER may be either a buffer or its name (a string)."
  (interactive (list (read-buffer "Kill buffer: " (current-buffer) 'existing)))
  (setq buffer  (get-buffer buffer))
  (if (buffer-live-p buffer)            ; Kill live buffer only.
      (let ((wins  (get-buffer-window-list buffer nil t))) ; On all frames.
        (when (and (buffer-modified-p buffer)
                   (fboundp '1on1-flash-ding-minibuffer-frame))
          (1on1-flash-ding-minibuffer-frame t)) ; Defined in `oneonone.el'.
        (when (kill-buffer buffer)      ; Only delete windows if buffer killed.
          (dolist (win  wins)           ; (User might keep buffer if modified.)
            (when (window-live-p win)
              ;; Ignore error, in particular,
              ;; "Attempt to delete the sole visible or iconified frame".
              (condition-case nil (delete-window win) (error nil))))))
    (when (interactive-p)
      (error "Cannot kill buffer.  Not a live buffer: `%s'" buffer))))

(substitute-key-definition 'kill-buffer 'kill-buffer-and-its-windows global-map)

;;; put the buffer from the selected window in next window, and vice versa
(defun swap-buffers-in-windows ()
  "Put the buffer from the selected window in next window, and vice versa"
  (interactive)
  (let* ((this (selected-window))
         (other (next-window))
         (this-buffer (window-buffer this))
         (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)
    )
  )

(global-set-key (kbd "C-x <down>") 'swap-buffers-in-windows)

;;; dired
(add-hook 'dired-load-hook
          '(lambda () (require 'dired-x)))
(add-hook 'dired-mode-hook
          (lambda () (dired-hide-details-mode 1)))
(setq dired-omit-mode t)
(setq dired-omit-files "\\.pdf$\\|\\.pyc$\\|\\.tern-port$")

;; set font size
(set-face-attribute 'default nil :family "Ubuntu Mono" :height 130)
(setq-default line-spacing 3)

;; arabic font, form:
;; "http://stackoverflow.com/questions/11012627/emacs-font-for-western-and-other-like-rtl"
(set-fontset-font
 "fontset-default"
 (cons (decode-char 'ucs #x0600) (decode-char 'ucs #x06ff))
 "DejaVu Sans Mono")

;;; install first-class packages
(defvar my-packages
  '(use-package auctex))

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

;; super-save
(use-package super-save
  :ensure t
  :config
  (super-save-initialize))

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
(setq my/ido-order '(".tex" ".go" ".js" ".el" ".py"))

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
  :diminish company-mode
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
    (setq projectile-keymap-prefix (kbd "C-c p"))
    (setq projectile-enable-caching t)
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

;; avy
(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "C-'") 'avy-goto-char))

;; ace-window
(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-x o") 'ace-window))

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
    ("use" "'use strict';")
    ("ar2" "⇒")))
(abbrev-mode 1)

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

;; python
(add-hook 'python-mode-hook
          (lambda ()
            (progn
              (pyvenv-mode)
              (flycheck-mode))))

;; geiser
(use-package geiser
  :ensure t)

;;;  golang

;; go-mode
(add-to-list 'load-path "~/src/github.com/dominikh/go-mode.el")
(require 'go-mode-autoloads)

                                        ; gocode
(add-to-list 'load-path "~/src/github.com/nsf/gocode/emacs-company")
(require 'company)
(require 'company-go)
(add-hook 'go-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends) '(company-go))
            (company-mode)))

;; golint
(add-to-list 'load-path
             (concat (getenv "GOPATH")
                     "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)

;; goimports
;; (setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

;; html
(use-package emmet-mode
  :ensure t
  :config
  (progn
    (add-hook 'html-mode-hook  'emmet-mode)))

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

;;; php
(use-package php-mode
  :ensure t
  :config
  (use-package ac-php
    :ensure t
    :config
    (progn
      (add-hook
       'php-mode-hook
       '(lambda ()
          (require 'ac-php-company)
          (company-mode t)
          (add-to-list 'company-backends 'company-ac-php-backend))))))


;;; docker
(use-package dockerfile-mode
  :ensure t)

(use-package restclient
  :ensure t)

;;; nginx
(use-package nginx-mode
  :ensure t)
;; normal size
;; (setq font-latex-fontify-sectioning 'color)
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
              ;; (flyspell-mode 1)
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
(add-to-list 'load-path
             "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas/minor-mode 1)

;;; key bindings
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "<escape>") (kbd "C-g"))
(global-set-key (kbd "C-<menu>") 'menu-bar-open)
(global-set-key (kbd "<M-up>") 'enlarge-window)
(global-set-key (kbd "<M-down>") 'shrink-window)
(global-set-key (kbd "C-,") 'shell-command)
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(writeroom-border-width 200)
 '(writeroom-maximize-window nil)
 '(writeroom-width 80))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(grizzl-selection-face ((t (:foreground "#8F9D6A")))))
