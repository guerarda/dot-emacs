(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'use-package)

;; No welcome page
(setq inhibit-startup-message t)

;; y-or-n prompt
(defalias 'yes-or-no-p 'y-or-n-p)

;; No ring bell
(setq ring-bell-function 'ignore)

;; Highlight current line
(global-hl-line-mode 1)

(setq ido-everywhere t)

;; Map 'cmd' to Meta and 'alt' to alt
(setq-default mac-option-key-is-meta nil)
(setq-default mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; Always ALWAYS use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(load-library "iso-transl")

(setenv "PATH"
        (concat (getenv "PATH")
                ":/usr/local/bin:/opt/local/bin"))
(setq exec-path (append exec-path
                        '("/usr/local/bin"
                          "/opt/local/bin")))

(bind-key "<C-M-return>" #'other-window)
(bind-key "C-c ;" #'comment-or-uncomment-region)

(defun my-prog-mode-hook ()
  (linum-mode)
  (flycheck-mode 1)
  (setq-default linum-format "%4d\u2502")
  (setq compilation-ask-about-save nil)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))
(add-hook 'prog-mode-hook 'my-prog-mode-hook)

(defun my-c-mode-hook ()
  (setq c-default-style "linux"
        c-basic-offset 4)
  (bind-key "C-c C-k" #'compile c-mode-base-map))
(add-hook 'c-mode-hook 'my-c-mode-hook)

(use-package better-defaults
  :ensure t)

(use-package cider
  :config
  (setq-default cider-show-error-buffer nil)
  (setq-default cider-stacktrace-fill-column 80))

;;(use-package company
;;  :config
;;  (add-hook 'prog-mode-hook 'company-mode))

(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

;; (use-package irony
;;   :config
;;   (add-hook 'c++-mode-hook 'irony-mode)
;;   (add-hook 'c-mode-hook 'irony-mode)
;;   (add-hook 'objc-mode-hook 'irony-mode))

 (use-package flycheck
   :bind ("C-c l" . flycheck-list-errors)
   :config
   (add-hook 'prog-mode-hook 'flycheck-mode)
   (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc
                                              emacs-lisp)))

(use-package magit
  :ensure t
  :init
  (use-package magit-svn
    :ensure t)
  :config
  (add-hook 'magit-mode-hook 'magit-svn-mode)
  :bind ("C-x g" . magit-status))

(use-package org
  :ensure t
  :bind ("C-c C-v k" . org-babel-remove-result)
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package paredit
  :ensure t
  :config
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package projectile
  :config
  (add-hook 'prog-mode-hook 'projectile-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command)))

(use-package solarized-theme
  :ensure t
  :config
  (setq solarized-use-variable-pitch nil
;        solarized-scale-org-headlines nil
        solarized-distinct-fring-background t
        solarized-high-contrast-mode-line t)
  (load-theme 'solarized-dark t))
