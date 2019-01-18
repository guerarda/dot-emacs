(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Ensure environment variables look the same as in the shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; No welcome page
(setq inhibit-startup-message t)

;; Initial frame position and size
(setq initial-frame-alist
      (append
       '((top . 0)
         (left . 600)
         (width . 100)
         (height . 80))
       initial-frame-alist))

;; Hiding toolbars
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

;; y-or-n prompt
(defalias 'yes-or-no-p 'y-or-n-p)

;; No ring bell
(setq ring-bell-function 'ignore)

(setq-default indent-tabs-mode nil)
(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      backup-directory-alist `(("." . ,(concat user-emacs-directory
					       "backups"))))

;; Map 'cmd' to Meta and 'alt' to alt
(setq-default mac-option-key-is-meta nil)
(setq-default mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; Always ALWAYS use UTF-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(setq-default ispell-program-name "/usr/local/bin/aspell")

;; Searching
(bind-key "C-s" #'isearch-forward-regexp)
(bind-key "C-r" #'isearch-backward-regexp)
(bind-key "C-M-s" #'isearch-forward)
(bind-key "C-M-r" #'isearch-backward)

(bind-key "C-x C-b" #'ibuffer)

(bind-key "C-c ;" #'comment-or-uncomment-region)
(bind-key "C-c o" #'whitespace-mode)
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; Splitting windows
(bind-key* "M-o" #'other-window)
(bind-key* "M-0" #'delete-window)
(bind-key* "M-1" #'delete-other-windows)
(bind-key* "M-2" #'split-window-vertically)
(bind-key* "M-3" #'split-window-horizontally)

;; C-a moves to first non-whitespace characted, then the real
;; beginning of line
(defadvice move-beginning-of-line (around smarter-bol activate)
  ;; Move to requested line if needed
  (let ((arg (or (ad-get-arg 0) 1)))
    (when (/= arg 1)
      (forward-line (1- arg))))
  ;; Move to indentation on first call, then to actual BOL on second.
  (let ((pos (point)))
    (back-to-indentation)
    (when (= pos (point))
      ad-do-it)))

(defun my-prog-mode-hook ()
  (nlinum-mode)
  (setq-default nlinum-format "%4d\u2502")
  (setq compilation-ask-about-save nil)
  (subword-mode 1)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))
(add-hook 'prog-mode-hook 'my-prog-mode-hook)

(defun my-c-mode-hook ()
  (setq c-default-style "linux"
        c-basic-offset 4)
  (c-set-offset 'case-label '0)
  (electric-pair-mode)
  (bind-key "C-c C-k" #'compile c-mode-base-map))
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

(require 'use-package)

(use-package ccls
  :config (setq ccls-executable "ccls"))

(use-package clang-format
  :after projectile
  :demand t
  :preface
  (defun clang-format-if-config ()
    "Run clang format only if a config file is present at the root of the project"
    (when (file-readable-p (expand-file-name ".clang-format" (projectile-project-root)))
      (clang-format-buffer)))
  (defun clang-format-buffer-if-config ()
    "Add to before-save hook"
    (add-hook 'before-save-hook 'clang-format-if-config nil t))
  :hook (c-mode-common . clang-format-buffer-if-config))

(use-package company
  :hook (prog-mode . company-mode))

(use-package company-lsp
  :after company
  :demand t
  :config
  (push 'company-lsp company-backends)
  (setq-default company-transformers nil)
  (setq-default company-lsp-async t)
  (setq-default company-lsp-cache-candidates nil))

(use-package counsel
  :after ivy
  :demand t
  :bind (("C-x C-f" . counsel-find-file)
         ("M-x" . counsel-M-x)))

(use-package counsel-projectile
  :after (counsel projectile)
  :demand t
  :config
  (counsel-projectile-mode 1))

(use-package flycheck
  :bind-keymap ("C-c f" . flycheck-command-map)
  :hook (prog-mode . flycheck-mode)
  :config
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [0 0 0 0 0 4 12 28 60 124 252 124 60 28 12 4 0 0 0 0])
  (setq-default flycheck-indication-mode 'right-fringe)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc
                                             emacs-lisp)))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode))

(use-package git-gutter-fringe
  :config
  (setq-default fringes-outside-margins t)
  (fringe-helper-define 'git-gutter-fr:added '(center repeated)
    "XXX.....")
  (fringe-helper-define 'git-gutter-fr:modified '(center repeated)
    "XXX.....")
  (fringe-helper-define 'git-gutter-fr:deleted 'bottom
    "X......."
    "XX......"
    "XXX....."
    "XXXX...."))

(use-package hl-line
  :config (global-hl-line-mode 1))

(use-package ido
  :disabled
  :config
  (ido-mode t)
  (setq ido-everywhere t)
  (setq ido-enable-flex-matching t))

(use-package ido-vertical-mode
  :disabled
  :ensure t
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package ivy
  :demand t
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window)
         ("M-H" . ivy-resume))
  :bind (:map ivy-minibuffer-map
              ("<tab>" . ivy-alt-done))
  :config
  (ivy-mode 1))

(use-package lsp
  :config
  (setq lsp-prefer-flymake nil)
  :hook (c-mode-common . lsp))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-ui-flycheck
  :after lsp-ui
  :demand t
  :hook (lsp-after-open . (lambda () (lsp-ui-flycheck-enable 1))))

;(add-hook 'lsp-after-open-hook (lambda () (lsp-ui-flycheck-enable 1)))
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :bind (:map magit-mode-map
         ("C-x 1" . magit-section-show-level-1-all)
         ("C-x 2" . magit-section-show-level-2-all)
         ("C-x 3" . magit-section-show-level-3-all)
         ("C-x 4" . magit-section-show-level-4-all))
  :config
  (delete 'Git vc-handled-backends)
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package misc
  :bind ("M-z" . zap-up-to-char))

(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package org
  :ensure t
  :bind (("C-c C-v k" . org-babel-remove-result)
         ("M-p" . org-metaup)
         ("M-n" . org-metadown)
         ("C-c c" . org-capture))
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

(use-package paredit
  :ensure t
  :hook ((clojure-mode . paredit-mode)
         (lisp-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)))

(use-package paren
  :config (show-paren-mode 1))

(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-global-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package saveplace
  :ensure t
  :config
  (save-place-mode 1)
  (setq save-place-file (concat user-emacs-directory "places")))

(use-package smex
  :disabled
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command)))

(use-package solarized-theme
  :ensure t
  :config
  (setq solarized-use-variable-pitch nil
        solarized-distinct-fring-background t
        solarized-high-contrast-mode-line t)
  (load-theme 'solarized-dark t))

(use-package swiper
  :after ivy
  :bind ("C-s" . swiper))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config (yas-load-directory "~/.emacs.d/snippets/"))

(use-package shackle
  :config
  (shackle-mode))

;; (defun my-irony-mode-hook ()
;;   (define-key irony-mode-map [remap completion-at-point]
;;     'irony-completion-at-point-async)
;;   (define-key irony-mode-map [remap complete-symbol]
;;     'irony-completion-at-point-async))
;; (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; (use-package cider
;;   :config
;;   (setq-default cider-show-error-buffer nil)
;;   (setq-default cider-stacktrace-fill-column 80))

;; (use-package intero
;;   :config
;;   (add-hook 'haskell-mode-hook 'intero-mode))

;; (use-package irony
;;   :config
;;   (add-hook 'c++-mode-hook 'irony-mode)
;;   (add-hook 'c-mode-hook 'irony-mode)
;;   (add-hook 'objc-mode-hook 'irony-mode))
