;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
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

;; Prefer UTF-8
(prefer-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; (set-default-coding-systems 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (setq-default buffer-file-coding-system 'utf-8-unix)

;; Searching
(bind-key "C-s" #'isearch-forward-regexp)
(bind-key "C-r" #'isearch-backward-regexp)
(bind-key "C-M-s" #'isearch-forward)
(bind-key "C-M-r" #'isearch-backward)

(bind-key "C-x C-b" #'ibuffer)

(bind-key "C-c e f" #'customize-face)
(bind-key "C-c e g" #'customize-group)

(bind-key "C-c ;" #'comment-or-uncomment-region)
(bind-key "C-c w o" #'whitespace-mode)
(bind-key "C-c w f" #'fixup-whitespace)

(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; Splitting windows
(bind-key* "M-o" #'other-window)
(bind-key* "M-0" #'delete-window)
(bind-key* "M-1" #'delete-other-windows)
(bind-key* "M-2" #'split-window-vertically)
(bind-key* "M-3" #'split-window-horizontally)

;; Major mode for .mm files is c++-mode
(add-to-list 'auto-mode-alist '("\\.mm\\'" . c++-mode))

(defun my-prog-mode-hook ()
  (nlinum-mode)
  (setq-default nlinum-format "%4d\u2502")
  (setq compilation-ask-about-save nil)
  (subword-mode 1)
  (electric-pair-local-mode)
  (add-hook 'before-save-hook 'whitespace-cleanup))
(add-hook 'prog-mode-hook 'my-prog-mode-hook)

(defun my-c-mode-hook ()
  (setq c-default-style "linux"
        c-basic-offset 4)
  (c-set-offset 'case-label '0)
  (bind-key "C-c C-c" #'compile c-mode-base-map)
  (bind-key "C-c c b" #'(lambda () (interactive) (swiper "#pragma mark"))))
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

;; (defun load-my-theme (frame)
;;   (select-frame frame)
;;   (load-theme 'solarized-dark t))

;; (if (daemonp)
;;     (add-hook 'after-make-frame-functions #'load-my-theme)
;;   (load-theme 'solarized-dark t))

;; use ' instead of quote when saving customization
(defadvice custom-save-all (around custom-save-all-around)
  "Use abbreviated quotes for customize."
  (let ((print-quoted t))
    ad-do-it))

(require 'use-package)

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

(use-package cmake-font-lock
  :after cmake-mode
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package company
  :bind ((:map company-search-map
                ("C-t" . company-search-toggle-filtering)
                ("C-n" . company-select-next)
                ("C-p" . company-select-previous))
         (:map company-active-map
               ("C-n" . company-select-next)
               ("C-p" . company-select-previous)))
  :hook (prog-mode . company-mode))

(use-package company-lsp
  :commands company-lsp)

(use-package counsel
  :after ivy
  :demand t
  :bind (("C-c i" . counsel-imenu)
         ("C-x C-f" . counsel-find-file)
         ("M-x" . counsel-M-x)))

(use-package counsel-projectile
  :after (counsel projectile)
  :demand t
  :config
  (counsel-projectile-mode 1)
  (setq counsel-projectile-switch-project-action 'counsel-projectile-switch-project-action-vc))

(use-package crux
  :bind (([remap kill-line] . crux-smart-kill-line)
         ("C-a" . crux-move-beginning-of-line)
         ("C-c e c" . crux-find-user-custom-file)
         ("C-c e i" . crux-find-user-init-file)
         ("C-c w u" . crux-upcase-region)
         ("C-c w l" . crux-downcase-region)
         ("C-c w p" . crux-capitalize-region)))

(use-package doom-modeline
  :config
  (setq doom-modeline-minor-modes t
        doom-modeline-buffer-modification-icon nil
        doom-modeline-buffer-file-name-style 'relative-from-project)
  :hook (after-init . doom-modeline-mode))

(use-package flycheck
  :bind-keymap ("C-c f" . flycheck-command-map)
  :hook (prog-mode . flycheck-mode)
  :config
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [0 0 0 0 0 4 12 28 60 124 252 124 60 28 12 4 0 0 0 0])
  (setq-default flycheck-indication-mode 'right-fringe)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc
                                             emacs-lisp)))

(use-package flyspell
  :if (executable-find "aspell")
  :hook ((text-mode . turn-on-flyspell))
  :init
  (setq-default ispell-program-name "aspell")
  (setq-default ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
  (setq-default flyspell-prog-text-faces '(font-lock-comment-face font-lock-doc-face)))

(use-package git-gutter
  :bind (("C-c g k" . git-gutter:revert-hunk)
         ("C-c g n" . git-gutter:next-hunk)
         ("C-c g p" . git-gutter:previous-hunk)
         ("C-c g s" . git-gutter:stage-hunk)
         ("C-c g d" . git-gutter:popup-hunk))
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

(use-package ispell
  :bind ("M-%" . ispell-word))

(use-package ivy
  :demand t
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window)
         ("M-H" . ivy-resume))
  :bind (:map ivy-minibuffer-map
              ("<tab>" . ivy-alt-done))
  :config
  (ivy-mode 1))

(use-package intero
  :hook (haskell-mode . intero-mode))

(use-package lsp-mode
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil)
  :hook ((c-mode-common . lsp)
         (python-mode . lsp)))

(use-package lsp-ui
  :commands lsp-ui-mode
  :bind ("C-c c i" . lsp-ui-imenu)
  :bind (:map lsp-ui-imenu-mode-map
              ("q" . lsp-ui-imenu--kill)
              ("n" . next-line)
              ("p" . previous-line)
              ("M-n" . lsp-ui-imenu--next-kind)
              ("M-p" . lsp-ui-imenu--prev-kind)
              ("<return>" . lsp-ui-imenu--visit)
              ("M-<return>" . lsp-ui-imenu--view)))

(use-package lsp-ui-flycheck
  :commands lsp-ui)

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
  (setq magit-completing-read-function 'ivy-completing-read)
  :hook (git-commit-setup . git-commit-turn-on-flyspell))

(use-package misc
  :bind ("M-z" . zap-up-to-char))

(use-package minions
  :config (minions-mode 1))

(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package org
  :ensure t
  :bind (("C-c o c" . org-capture)
         ("C-c o n" . (lambda () (interactive) (find-file-other-window "~/Documents/notes.org"))))
  (:map org-mode-map
         ("C-c C-v k" . org-babel-remove-result)
         ("M-p" . org-metaup)
         ("M-n" . org-metadown)))

(use-package org-bullets-mode
  :hook (org-mode . org-bullets-mode))

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

(use-package python-black
  :demand t
  :after python
  ;:hook (python-mode . python-black-on-save-mode)
)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package saveplace
  :ensure t
  :config
  (save-place-mode 1)
  (setq save-place-file (concat user-emacs-directory "places")))

(use-package solarized-theme
  :ensure t
  :config
  (setq solarized-use-variable-pitch nil
        solarized-distinct-fring-background t
        solarized-high-contrast-mode-line t)
  (load-theme 'solarized-dark t))

(use-package swiper
  :after ivy
  :init (defun swiper-at-point ()
          (interactive)
          (swiper (thing-at-point 'symbol)))
  :bind (("C-s" . swiper-isearch)
         ("C-c s" . swiper-at-point)))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config (yas-load-directory "~/.emacs.d/snippets/"))

(use-package shackle
  :config
  (shackle-mode))

;; (use-package cider
;;   :config
;;   (setq-default cider-show-error-buffer nil)
;;   (setq-default cider-stacktrace-fill-column 80))

(use-package web-mode
  :hook (web-mode . (lambda () (electric-pair-local-mode -1)))
  :init (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  :config
  (setq web-mode-engines-alist
                '(("django" . "\\.html\\'"))))
