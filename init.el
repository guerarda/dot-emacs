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

;; Do not show those confusing warnings when installing packages
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

;; Hiding toolbars
(tool-bar-mode -1)
(menu-bar-mode -1)
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

(defun unfill-paragraph (beg end &optional copy-only)
  "Remove line breaks in the region from BEG to END,
preserving empty lines as paragraph separators.
By default, modifies the buffer directly without copying.
When called with a prefix argument, only copies to the kill ring without modifying."
  (interactive "r\nP")
  (let ((text (buffer-substring-no-properties beg end))
        (result "")
        (current-paragraph ""))

    ;; Process the text line by line
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))

      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))

          (cond
           ;; Empty line - add to result and reset current paragraph
           ((string-match-p "^\\s-*$" line)
            (when (not (string= current-paragraph ""))
              (setq result (concat result current-paragraph "\n")))
            (setq result (concat result "\n"))
            (setq current-paragraph ""))

           ;; Regular line - append to current paragraph with space
           (t
            (if (string= current-paragraph "")
                (setq current-paragraph line)
              (setq current-paragraph (concat current-paragraph " " line))))))

        (forward-line 1))

      ;; Add final paragraph if any
      (when (not (string= current-paragraph ""))
        (setq result (concat result current-paragraph))))

    (if copy-only
        ;; If universal argument provided, only copy to kill ring
        (kill-new result)
      ;; Default behavior: modify buffer without copying
      (delete-region beg end)
      (insert result))))
(bind-key "C-c u" #'unfill-paragraph)


(defun my-c-mode-hook ()
  (setq c-default-style "linux"
        c-basic-offset 4)
  (c-set-offset 'case-label '0)
  (bind-key "C-c C-c" #'compile c-mode-base-map)
  (bind-key "C-c c b" #'(lambda () (interactive) (swiper "#pragma mark"))))
(add-hook 'c-mode-common-hook 'my-c-mode-hook)


(defun my-keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(bind-key "C-g" #'my-keyboard-quit-dwim)

(defun ag/auto-commit ()
  (interactive)
  (start-process "gcauto" "*Messages*" shell-file-name "-ic" "gcauto"))

(defun ag/insert-now-timestamp ()
  "Insert current timestamp at point."
  (interactive)
  (insert (format-time-string "%a %b %e %H:%M:%S %Z %Y")))

(defun my-project-flush-lines (regex file-extension)
  "Flush lines matching REGEX in all project files with FILE-EXTENSION.
Similar to `flush-lines` but operates on all project files."
  (interactive
   (list
    (read-regexp "Flush lines matching regex: ")
    (read-string "File extension (e.g. el, py): ")))

  (let* ((project (project-current t))
         (files (project-files project))
         (ext-regexp (concat "\\." file-extension "$"))
         (matching-files (seq-filter (lambda (file)
                                       (string-match-p ext-regexp file))
                                     files))
         (count 0))

    (dolist (file matching-files)
      (with-current-buffer (find-file-noselect file)
        (let ((original-point (point))
              (original-modified (buffer-modified-p))
              (lines-removed 0))
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward regex nil t)
              (beginning-of-line)
              (kill-line 1)
              (setq lines-removed (1+ lines-removed))))

          (when (> lines-removed 0)
            (setq count (+ count lines-removed))
            (message "Removed %d lines from %s" lines-removed file)
            (save-buffer))

          (unless original-modified
            (set-buffer-modified-p nil))
          (goto-char original-point))))

    (message "Flushed %d lines total across %d files" count (length matching-files))))

(defun my-project-flush-lines-dwim ()
  "DWIM version of `project-flush-lines`.
Uses the word at point as regex and current buffer's extension."
  (interactive)
  (let* ((word (thing-at-point 'word t))
         (file-name (buffer-file-name))
         (ext (and file-name
                  (file-name-extension file-name))))
    (if (and word ext)
        (my-project-flush-lines word ext)
      (call-interactively 'my-project-flush-lines))))

(defun my-project-flush-debug-comments-dwim ()
  "Remove all lines with DEBUG comments in project files of same type as current buffer.
Uses the appropriate comment syntax for the current major mode."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (ext (and file-name (file-name-extension file-name)))
         (debug-pattern "\\s-*DEBUG\\b")
         (case-fold-search nil)
         (comment-pattern (concat comment-start-skip debug-pattern)))
    (if (not ext)
        (message "Buffer has no file extension.")
      (message "Removing DEBUG comments using pattern: %s" comment-pattern)
      (my-project-flush-lines comment-pattern ext)
      (message "Finished removing DEBUG comments from all %s files in project." ext))))

;; Copy buffer file name to kill ring
(bind-key* "C-c C-f"
           (lambda (arg)
             (interactive "P")
             (when-let ((filename (buffer-file-name)))
               (kill-new (if arg
                             filename
                           (file-name-nondirectory filename))))))

;; Programming related major modes
;;
(use-package prog-mode
  :straight (:type built-in)
  :hook (prog-mode . (lambda ()
                       (display-line-numbers-mode)
                       (setq compilation-ask-about-save nil)
                       (subword-mode 1)
                       (electric-pair-local-mode))))

(use-package c++-ts-mode
  :straight (:type built-in)
  :mode (("\\.cpp\\'" . c++-ts-mode)
         ("\\.h\\'" . c++-ts-mode))
  :init (setq c-ts-mode-indent-offset 4))

(use-package js-ts-mode
  :straight (:type built-in)
  :mode ("\\.js\\'" . js-ts-mode)
  :init (setq js-indent-level 2))

(use-package json-ts-mode
  :straight (:type built-in)
  :mode ("\\.json\\'" . json-ts-mode)
  :init (setq json-ts-mode-indent-offset 2))

(use-package typescript-ts-mode
  :straight (:type built-in)
  :mode ("\\.ts\\'" . typescript-ts-mode)
  :custom (typescript-ts-mode-indent-offset 2))

(use-package css-ts-mode
  :straight (:type built-in)
  :mode ("\\.css\\'" . css-ts-mode)
  :custom (css-indent-offset 2))

(use-package python-ts-mode
  :straight (:type built-in)
  :mode ("\\.py\\'" . python-ts-mode))

(use-package rust-ts-mode
  :straight (:type built-in)
  :mode ("\\.rs\\'" . rust-ts-mode))

;; Packages
;;
(use-package aidermacs
  :straight (:host github :repo "MatthewZMD/aidermacs" :files ("*.el"))
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  (aidermacs-setup-minor-mode)
  (setq aidermacs-use-architect-mode t))

(use-package autorevert
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq auto-revert-verbose t))

(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package beancount-mode
  :straight (beancount-mode :type git :host github :repo "beancount/beancount-mode")
  :bind (:map beancount-mode-map
              ("M-n" . beancount-goto-next-transaction)
              ("M-p" . beancount-goto-previous-transaction))
  :hook ((beancount-mode . (lambda () (electric-indent-local-mode -1)))
         (beancount-mode . outline-minor-mode)) )

(use-package cape)

(use-package cmake-font-lock
  :after cmake-mode
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package compile
  :custom
  ((compilation-always-kill t)
   (compilation-scroll-output 'first-error)))

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x f" . consult-recent-file)
         ("C-x p b" . consult-project-buffer)
         ("C-x r b" . consult-bookmark)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g g" . consult-goto-line)
         ("M-y" . consult-yank-pop)
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-'" . consult-register)
         ("M-s i" . consult-info)
         ("M-g i" . consult-imenu)
         ("M-g ." . consult-xref)
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
               ("M-r" . consult-history)))
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :custom
  (consult-line-start-from-top t))

(use-package consult-dir
  :after consult
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :custom
  (corfu-max-width 80)
  (corfu-min-width 40))

(use-package corfu
  :after cape
  :bind (:map corfu-map
              ("C-SPC" . corfu-insert-separator))
  :custom
  (corfu-separator ?\s)
  :hook
  ((prog-mode . corfu-mode)
   (shell-mode . corfu-mode))
  :init
  (global-corfu-mode)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

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

(use-package delsel
  :straight (:type built-in)
  :hook (after-init . delete-selection-mode))

(use-package diff-hl
  :after magit
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :init
  (global-diff-hl-mode))

(use-package dired
  :straight (:type built-in)
  :bind (:map dired-mode-map
              ("a" . dired-jump)
              ("M-s" . nil)))

(use-package dired-narrow
  :after dired
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("TAB" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-remove)
              ("S-TAB" . dired-subtree-remove)))

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

(use-package eat
  :straight (:type git
             :host codeberg
             :repo "akib/emacs-eat"
             :files ("*.el" ("term" "term/*.el") "*.texi"
                     "*.ti" ("terminfo/e" "terminfo/e/*")
                     ("terminfo/65" "terminfo/65/*")
                     ("integration" "integration/*")
                     (:exclude ".dir-locals.el" "*-tests.el"))))

(use-package eglot
  :init
  (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))
  :hook
  ((css-ts-mode
    js-ts-mode
    json-ts-mode
    python-ts-mode
    typescript-ts-mode
    rust-ts-mode) . eglot-ensure))

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (which-key-mode t)
  :config
  (setq truncate-lines t)
  :bind (("M-z" . zap-up-to-char)
         ("C-x C-b" . ibuffer)
         ("C-c d" . duplicate-dwim)))

(use-package embark
  :bind* (("C-;" . embark-act)
          ("M-." . embark-dwim)))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package exec-path-from-shell
  ;; Ensure environment variables look the same as in the shell
  :init
  (when (daemonp)
    (exec-path-from-shell-initialize))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package fd-dired
  :bind (("C-c s f" . fd-dired)))

(use-package gptel
  :init
  (setq gptel-model 'claude-sonnet-4-20250514
        gptel-backend (gptel-make-anthropic "Claude"
                                            :stream t :key gptel-api-key))
  (gptel-make-gemini "Gemini" :stream t :key gptel-api-key)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  :custom
  (gptel-default-mode 'org-mode))

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

(use-package jinx
  :after vertico
  :hook ((text-mode . jinx-mode))
  :bind (("M-i" . jinx-correct))
  :config
  (unbind-key "M-$" jinx-correct)
  :custom
  (jinx-camel-modes '(prog-mode))
  (jinx-delay 0.1))

(use-package llvm-ts-mode)

(use-package magit
  :bind (("C-x g" . magit-status))
  :bind (:map magit-mode-map
              ("C-x 1" . magit-section-show-level-1-all)
              ("C-x 2" . magit-section-show-level-2-all)
              ("C-x 3" . magit-section-show-level-3-all)
              ("C-x 4" . magit-section-show-level-4-all))
  :custom
  (magit-git-executable "git")
  (magit-section-visibility-indicators '((magit-fringe-bitmap> . magit-fringe-bitmapv) (">" . t))))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotator-light nil))
 :init
 (marginalia-mode))

(use-package markdown-mode)

(use-package modern-cpp-font-lock
  :hook (c++-ts-mode . modern-c++-font-lock-mode))

(use-package ob-racket
  :after org
  :config
  (add-hook 'ob-racket-pre-runtime-library-load-hook
              #'ob-racket-raco-make-runtime-library)
  :straight (ob-racket
               :type git :host github :repo "hasu/emacs-ob-racket"
               :files ("*.el" "*.rkt")))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package org
  :preface
  (defun ag/org-insert-heading (arg)
    (interactive "P")
    (if arg (org-insert-subheading arg)
      (org-insert-heading)))

  (defun ag/org-insert-todo-heading (arg)
    (interactive "P")
    (if arg (org-insert-todo-subheading arg)
      (org-insert-todo-heading arg)))

  :bind (("C-c o a" . org-agenda)
         ("C-c o c" . org-capture)
         ("C-c o l" . org-store-link)
         ("C-c o n" . (lambda () (interactive) (find-file-other-window "~/Desktop/org/notes.org")))
         ("C-c o t" . (lambda () (interactive) (find-file-other-window "~/Desktop/org/todo.org")))
         ("C-c o b" . (lambda () (interactive) (let ((default-directory "~/Desktop/org/"))
                                                 (call-interactively 'find-file))))
         )
  (:map org-mode-map
        ("C-c o u" . org-move-subtree-up)
        ("C-c o d" . org-move-subtree-down)
        ("C-c o p" . org-promote-subtree)
        ("C-c o m" . org-demote-subtree)
        ("C-c o k" . org-cut-subtree)
        ("C-c C-." . org-time-stamp-inactive)
        ("C-c o e" . org-emphasize)
        ("C-c o i d" . org-insert-drawer)
        ;("C-c o i s" . org-insert-subheading)
        ("C-c o i h" . ag/org-insert-heading)
        ("C-c o i t" . ag/org-insert-todo-heading)
        ("C-c o i m" . (lambda () (interactive)
                         (progn
                           (org-insert-heading (org-current-level))
                           (org-insert-time-stamp (current-time) t t nil nil nil))))
        ("M-g o" . consult-org-heading)
        ("C-c C-v k" . org-babel-remove-result-one-or-many)
        ("M-p" . org-metaup)
        ("M-n" . org-metadown))
  :custom
  (org-startup-indented t)
  :config
  (subword-mode 1)
  (setq org-agenda-files '("~/Desktop/org")))

(use-package orgit)

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

(use-package nerd-icons)

(use-package nerd-icons-completion
  :after marginalia nerd-icons
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :after corfu nerd-icons
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :after dired nerd-icons
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package paredit
  :bind (:map paredit-mode-map
              ("M-s" . nil)
              ("M-p" . paredit-splice-sexp)
              ("C-`" . paredit-splice-sexp))
  :hook ((clojure-mode . paredit-mode)
         (lisp-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)))

(use-package paren
  :config (show-paren-mode 1))

(use-package racket-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package reformatter
  :init
  (reformatter-define prettierjs-format
                      :program "prettier"
                      :args (list "--stdin-filepath" buffer-file-name)
                      :lighter " PrtFmt")
  (reformatter-define ruff-format
                      :program "ruff"
                      :args (list "format" "--stdin-filename" buffer-file-name "-")
                      :lighter " fmt")
  (reformatter-define ruff-sort-imports
                      :program "ruff"
                      :args (list "check" "--select" "I" "--fix" "--stdin-filename" buffer-file-name "-")
                      :lighter " isort")
  (reformatter-define rust-format
                      :program  "rustfmt"
                      :lighter " fmt"))

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

(use-package tempel
  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      (remq #'tempel-expand completion-at-point-functions))))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  (add-hook 'eglot-managed-mode-hook 'tempel-setup-capf)
  :custom (tempel-path "~/.emacs.d/tempel-templates.eld")
  :bind (:map tempel-map
              ("TAB" . tempel-next)
              ("S-TAB" . tempel-previous)))

(use-package treesit
  :straight (:type built-in)
  :mode (("\\.c\\'" . c-ts-mode)
         ("\\.cpp\\'" . c++-ts-mode)
         ("\\.h\\'" . c-or-c++-ts-mode)
         ("\\.hpp\\'" . c++-ts-mode))
  :preface
  (defun ag/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             ;; Note the version numbers. These are the versions that
             ;; are known to work with Combobulate *and* Emacs.
             '((c . ("https://github.com/tree-sitter/tree-sitter-c"))
               (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
               (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.23.2"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (wgsl . ("https://github.com/szebniok/tree-sitter-wgsl"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
               (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile" "v0.2.0"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (html-mode . html-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode . js-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (ag/setup-install-grammars))

(use-package uniquify
  :straight (:type built-in)
  :config (setq uniquify-buffer-name-style 'forward))

(use-package vertico
  :init
  (vertico-mode)
  (add-to-list 'vertico-multiform-categories
               '(buffer (vertico-sort-function . nil))
               '(jinx grid (vertico-grid-annotate . 20) (vertico-count . 4)))
  (vertico-multiform-mode)
  :custom
  (vertico-count 20))

(use-package wat-ts-mode)

(use-package csv-mode)

(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

  :config
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4))

(use-package wgsl-ts-mode
  :straight (:type git :host github :repo "acowley/wgsl-ts-mode"))
