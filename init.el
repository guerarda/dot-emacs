;; Straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; No welcome page
(setq inhibit-startup-message t)

;;Scratch buffer configuration
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message "\
# This buffer is for notes you don't want to save.
# If you want to create a file, visit that file with C-x C-f,
# then enter the text in that file's own buffer.")

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

(bind-key "C-." #'execute-extended-command)

(bind-key "C-c e f" #'customize-face)
(bind-key "C-c e g" #'customize-group)
(bind-key "C-c ;" #'comment-or-uncomment-region)
(bind-key "C-c w o" #'whitespace-mode)
(bind-key "C-c w f" #'fixup-whitespace)
(bind-key "C-c w r" #'fill-region)

;; Splitting windows
(bind-key* "M-o" #'other-window)
(bind-key* "M-0" #'delete-window)
(bind-key* "M-1" #'delete-other-windows)
(bind-key* "M-2" #'split-window-vertically)
(bind-key* "M-3" #'split-window-horizontally)

;; use ' instead of quote when saving customization
(defadvice custom-save-all (around custom-save-all-around)
  "Use abbreviated quotes for customize."
  (let ((print-quoted t))
    ad-do-it))

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
(bind-key "M-Q" #'unfill-paragraph)

(defun my-prog-mode-hook ()
  (display-line-numbers-mode)
  (setq compilation-ask-about-save nil)
  (subword-mode 1)
  (electric-pair-local-mode))
(add-hook 'prog-mode-hook 'my-prog-mode-hook)

(defun my-c-mode-hook ()
  (setq c-default-style "linux"
        c-basic-offset 4)
  (c-set-offset 'case-label '0)
  (bind-key "C-c C-c" #'compile c-mode-base-map)
  (bind-key "C-c c b" #'(lambda () (interactive) (swiper "#pragma mark"))))
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

(defun my-js-mode-hook ()
  (setq indent-tabs-mode t
        js-indent-level 4
        tab-width 4)
  (hs-minor-mode))
(add-hook 'js-mode-hook 'my-js-mode-hook)

;; Copy buffer file name to kill ring
(bind-key* "C-c C-f" #'(lambda () (interactive) (kill-new (with-output-to-string (princ (buffer-file-name))))))

;;Packages
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

(use-package consult
  :demand t
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x f" . consult-recent-file)
         ("C-x r b" . consult-bookmark)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g g" . consult-goto-line)
         ("M-y" . consult-yank-pop)
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ("M-s i" . consult-info)
         ([remap Info-search] . consult-info)
         (:map search-map
               (("f" . consult-fd)
                ("r" . consult-ripgrep)
                ("l" . consult-line)
                ("L" . consult-line-multi)
                ("k" . consult-keep-lines)
                ("u" . consult-focus-lines)))
         (:map minibuffer-local-map
               ("M-s" . consult-history)
               ("M-r" . consult-history))))

(use-package consult-dir
  :after consult
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package crux
  :bind (([remap kill-line] . crux-smart-kill-line)
         ("C-a" . crux-move-beginning-of-line)
         ("C-c e c" . crux-find-user-custom-file)
         ("C-c e i" . crux-find-user-init-file)
         ("C-c w u" . crux-upcase-region)
         ("C-c w l" . crux-downcase-region)
         ("C-c w p" . crux-capitalize-region)))

(use-package deft
  :after org
  :init
  (defun cm/deft-parse-title (file contents)
    "Parse the given FILE and CONTENTS and determine the title.
  If `deft-use-filename-as-title' is nil, the title is taken to
  be the first non-empty line of the FILE.  Else the base name of the FILE is
  used as title."
    (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
      (if begin
          (string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
        (deft-base-filename file))))
  (advice-add 'deft-parse-title :override #'cm/deft-parse-title)
  :bind ("C-c n d" . deft)
  :custom
  ;; (deft-strip-summary-regexp "\\`\\(.+\n\\)+\n")
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/Desktop/org/"))

(use-package dired
  :straight (:type built-in)
  :bind (:map dired-mode-map
              ("a" . dired-jump)))

(use-package dired-narrow
  :after dired
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              ("M-i" . dired-subtree-remove)))

(use-package doom-modeline
  :config
  (setq doom-modeline-minor-modes t
        doom-modeline-buffer-modification-icon nil
        doom-modeline-buffer-file-name-style 'relative-from-project)
  :hook (after-init . doom-modeline-mode))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package emacs
  :config
  (setq truncate-lines t))

(use-package embark
  :bind (("C-;" . embark-act)
         ("M-." . enbark-dwim)))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package exec-path-from-shell
  ;; Ensure environment variables look the same as in the shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package fd-dired
  :bind (("C-c s f" . fd-dired)))

;; (use-package flyspell
;;   :hook ((text-mode . turn-on-flyspell))
;;   :init
;;   (setq ispell-program-name "hunspell")
;;   (setq ispell-hunspell-dict-paths-alist
;;         '(("en_US" "C:/Users/aguerard/.hunspell/en_US.aff")))
;;   (setq ispell-local-dictionary "en_US")
;;   (setq ispell-local-dictionary-alist
;;         ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
;;         ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
;;         '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
;;   :bind ("M-i" . flyspell-auto-correct-word)
;;   :config
;;   (unbind-key "C-." flyspell-mode-map))


(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h k" . helpful-key)
         ("C-h v" . helpful-variable)
         ("C-c C-d" . helpful-at-point)))

(use-package hideshow
  :straight (:type built-in)
  :bind ("<backtab>" . hs-cycle)
  :init
  (defun hs-cycle (&optional level)
  (interactive "p")
  (let (message-log-max
        (inhibit-message t))
    (if (= level 1)
        (pcase last-command
          ('hs-cycle
           (hs-hide-level 1)
           (setq this-command 'hs-cycle-children))
          ('hs-cycle-children
           ;; TODO: Fix this case. `hs-show-block' needs to be
           ;; called twice to open all folds of the parent
           ;; block.
           (save-excursion (hs-show-block))
           (hs-show-block)
           (setq this-command 'hs-cycle-subtree))
          ('hs-cycle-subtree
           (hs-hide-block))
          (_
           (if (not (hs-already-hidden-p))
               (hs-hide-block)
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))))
      (hs-hide-level level)
      (setq this-command 'hs-hide-level)))))

(use-package hl-line
  :config (global-hl-line-mode 1))


(use-package ispell
  :bind ("M-%" . ispell-word))

(use-package magit
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

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotator-light nil))
 :init
 (marginalia-mode))

(use-package markdown-mode)

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package orderless
  :init
    (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package org
  :after consult
  :bind (("C-c o a" . org-agenda)
         ("C-c o c" . org-capture)
         ("C-c o l" . org-store-link)
         ("C-c o n" . (lambda () (interactive) (find-file-other-window "~/Desktop/org/notes.org")))
         ("C-c o t" . (lambda () (interactive) (find-file-other-window "~/Desktop/org/todo.org"))))
  (:map org-mode-map
        ("C-c C-." . org-time-stamp-inactive)
        ("C-c o e" . org-emphasize)
        ("C-c o i d" . org-insert-drawer)
        ("C-c o i s" . org-insert-subheading)
        ("C-c o i h" . org-insert-heading)
                ("C-c o o" . consult-org-heading)
        ("C-c C-v k" . org-babel-remove-result)
        ("M-p" . org-metaup)
        ("M-n" . org-metadown))
  :custom
  (org-startup-indented t)
  :config
  (subword-mode 1)
  (setq org-agenda-files '("~/Desktop/org")))

(use-package org-bullets-mode
  :disabled t
  :hook (org-mode . org-bullets-mode))

(use-package org-journal
  :preface
  (defun org-journal-file-header-func (time)
  "Custom function to create journal header."
  (concat
    (pcase org-journal-file-type
      (`daily (format-time-string "#+TITLE: %A, %e %B %Y\n#+STARTUP: showeverything\n"))
      (`weekly (format-time-string "#+TITLE: Week %W %Y\n#+STARTUP: folded\n"))
      (`monthly (format-time-string "#+TITLE: %B %Y\n#+STARTUP: folded\n") )
      (`yearly (format-time-string "#+TITLE: %Y Yearly Journal\n#+STARTUP: folded\n")))))
  :bind (("C-c o j o" . org-journal-open-current-journal-file)
         ("C-c o j n" . org-journal-new-entry))
  :config (setq org-journal-file-header 'org-journal-file-header-func))

(use-package outline
  :after consult
  :bind (:map outline-mode-map
              ("C-c o o" . consult-outline)))

(use-package p4
  :bind (("C-x p e" . p4-edit)
         ("C-x p d" . p4-diff)
         ("C-x p =" . p4-diff2)))

(use-package paredit
  :hook ((clojure-mode . paredit-mode)
         (lisp-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)))

(use-package paren
  :config (show-paren-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package recentf
  :config
  (recentf-mode 1))

(use-package rg
  :bind (("C-c s r" . rg-menu)
         ("C-c s d" . rg-dwim))
  :init
  (transient-insert-suffix 'rg-menu "-n" '(1 "-l" "Filenames only" "--files-with-matches"))
  (rg-define-toggle "--files-with-matches" "F" nil)
  (rg-define-toggle "--sort path" "s" nil)
  :config
  (setq rg-custom-type-aliases
        '(("bff" . "*.bff")))
  :custom (rg-executable "rg"))

(use-package saveplace
  :config
  (save-place-mode 1)
  (setq save-place-file (concat user-emacs-directory "places")))

(use-package simple
  :straight (:type built-in)
  :bind* (("M-l" . downcase-dwim)
          ("M-u" . upcase-dwim)
          ("M-c" . capitalize-dwim)))

(use-package uniquify
  :straight (:type built-in)
  :config (setq uniquify-buffer-name-style 'forward))

(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-count 20))
