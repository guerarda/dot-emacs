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
(setq-default buffer-file-coding-system 'utf-8-unix)

(setenv "PATH"
        (concat (getenv "PATH")
                ":/usr/local/bin:/opt/local/bin"))
(setq exec-path (append exec-path
                        '("/usr/local/bin"
                          "/opt/local/bin")))

(bind-key "M-o" #'other-window)
(bind-key "C-c ;" #'comment-or-uncomment-region)
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

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
  (add-hook 'before-save-hook 'delete-trailing-whitespace))
(add-hook 'prog-mode-hook 'my-prog-mode-hook)

(defun my-c-mode-hook ()
  (setq c-default-style "linux"
        c-basic-offset 4)
  (electric-pair-mode)
  (bind-key "C-c C-k" #'compile c-mode-base-map))
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

(use-package better-defaults
  :ensure t)

(use-package cider
  :config
  (setq-default cider-show-error-buffer nil)
  (setq-default cider-stacktrace-fill-column 80))

(use-package company
 :config
 (add-hook 'prog-mode-hook 'company-mode))

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

;; (defun my-irony-mode-hook ()
;;   (define-key irony-mode-map [remap completion-at-point]
;;     'irony-completion-at-point-async)
;;   (define-key irony-mode-map [remap complete-symbol]
;;     'irony-completion-at-point-async))
;; (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

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

(use-package modern-cpp-font-lock
  :ensure t
  :config
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

(use-package intero
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package org
  :ensure t
  :bind (("C-c C-v k" . org-babel-remove-result)
         ("M-p" . org-metaup)
         ("M-n" . org-metadown))
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
;;        solarized-scale-org-headlines nil
        solarized-distinct-fring-background t
        solarized-high-contrast-mode-line t)
  (load-theme 'solarized-dark t))
