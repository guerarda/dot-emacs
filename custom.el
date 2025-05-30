(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(corfu-auto t)
 '(deft-auto-save-interval 0.0)
 '(deft-use-filename-as-title nil)
 '(diff-font-lock-refine nil)
 '(dired-dwim-target t)
 '(ediff-diff-options "-w")
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eglot-autoshutdown t)
 '(eglot-code-action-indications '(eldoc-hint))
 '(eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
 '(electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
 '(electric-pair-mode t)
 '(elfeed-search-filter "@6-months-ago -archive")
 '(elfeed-search-remain-on-entry t)
 '(enable-recursive-minibuffers t)
 '(flycheck-display-errors-function 'flycheck-display-error-messages-unless-error-list)
 '(frame-resize-pixelwise t)
 '(git-commit-summary-max-length 72)
 '(ispell-dictionary "en_US")
 '(list-matching-lines-default-context-lines 2)
 '(llvm-ts-mode-indent-level 4)
 '(magit-section-visibility-indicator '(magit-fringe-bitmap> . magit-fringe-bitmapv))
 '(markdown-command "pandoc")
 '(org-agenda-files '("~/Desktop/org"))
 '(org-babel-load-languages
   '((emacs-lisp . t) (awk . t) (C . t) (shell . t) (python . t)
     (racket . t)))
 '(org-blank-before-new-entry '((heading . auto) (plain-list-item . auto)))
 '(org-bookmark-names-plist nil)
 '(org-capture-templates
   '(("b" "Add a book entry" entry
      (file+headline "~/Desktop/org/books.org" "2025")
      "** %^{Book title}\12:PROPERTIES:\12:Title:    %\\1\12:Author:   %^{Author}\12:Year:     %^{Year}\12:Started:  %^u\12:Finished:\12:END:"
      :empty-lines-after 1 :kill-buffer t)
     ("t" "Add a TODO entry" entry (file "~/Desktop/org/todo.org")
      "* TODO %^{Title}\12:LOGBOOK:\12- Created on %U\12:END:\12%?"
      :empty-lines-before 1 :empty-lines-after 1 :kill-buffer t
      :prepend t)
     ("n" "Add a note entry" entry (file "~/Desktop/org/notes.org")
      "* %^{Title}\12:LOGBOOK:\12- Created on %U\12:END:\12%?"
      :empty-lines-after 1 :prepend t)))
 '(org-confirm-babel-evaluate nil)
 '(org-fontify-whole-heading-line nil)
 '(org-hide-leading-stars t)
 '(org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
 '(org-journal-date-format "%A, %B %e %Y")
 '(org-journal-dir "~/Desktop/org/")
 '(org-journal-file-format "%Y-%m-%d.org")
 '(org-journal-file-header "#+title: ")
 '(org-journal-file-type 'yearly)
 '(org-log-into-drawer t)
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-targets '((org-agenda-files :maxlevel . 3)))
 '(org-refile-use-outline-path 'file)
 '(org-reverse-note-order t)
 '(org-roam-capture-templates
   '(("d" "default" plain "%?" :target
      (file+head "${slug}.org" "#+title: ${title}\12") :unnarrowed t)))
 '(org-src-fontify-natively t)
 '(org-src-lang-modes
   '(("json" . json-ts-mode) ("python" . python) ("C" . c) ("C++" . c++)
     ("asymptote" . asy) ("beamer" . latex) ("calc" . fundamental)
     ("cpp" . c++) ("ditaa" . artist) ("desktop" . conf-desktop)
     ("dot" . fundamental) ("elisp" . emacs-lisp) ("ocaml" . tuareg)
     ("screen" . shell-script) ("sqlite" . sql) ("toml" . conf-toml)
     ("shell" . sh) ("ash" . sh) ("sh" . sh) ("bash" . sh)
     ("jsh" . sh) ("bash2" . sh) ("dash" . sh) ("dtksh" . sh)
     ("ksh" . sh) ("es" . sh) ("rc" . sh) ("itcsh" . sh) ("tcsh" . sh)
     ("jcsh" . sh) ("csh" . sh) ("ksh88" . sh) ("oash" . sh)
     ("pdksh" . sh) ("mksh" . sh) ("posix" . sh) ("wksh" . sh)
     ("wsh" . sh) ("zsh" . sh) ("rpm" . sh)))
 '(org-src-preserve-indentation t)
 '(org-src-tab-acts-natively t)
 '(org-todo-keyword-faces
   '(("TODO" . "red") ("PROGRESS" . "orange") ("WAITING" . "cyan")
     ("REVIEW" . "yellow") ("CANCELLED" . "pale green")
     ("DONE" . "dark green")))
 '(org-todo-keywords
   '((sequence "TODO(t)" "PROGRESS(p)" "WAITING(w)" "REVIEW(r)" "|"
               "CANCELLED(c)" "DONE(d)")))
 '(orgit-store-reference t)
 '(project-switch-commands
   '((project-find-file "Find file" 102)
     (project-find-dir "Find directory" 100)
     (rg-project "rg project" 115) (magit-project-status "Magit" 109)
     (project-shell "Shell" 99)))
 '(project-vc-extra-root-markers '("package.json"))
 '(python-shell-interpreter "python3")
 '(reb-re-syntax 'string)
 '(recentf-max-menu-items 20)
 '(recentf-max-saved-items 50)
 '(rg-keymap-prefix "\3r")
 '(safe-local-variable-values
   '((eval python-black-on-save)
     (eval progn
           (defun run-interpreter nil
             (interactive)
             (let ((buffer (get-buffer-create "*Lox Interpreter*")))
               (pop-to-buffer buffer)
               (when (get-buffer-process buffer)
                 (delete-process (get-buffer-process buffer)))
               (make-comint-in-buffer "Lox Interpreter" buffer
                                      "python" nil "lox.py"))))))
 '(sentence-end-double-space nil)
 '(set-mark-command-repeat-pop t)
 '(visible-bell nil)
 '(wat-ts-mode-indent-level 4)
 '(whitespace-line-column 80)
 '(whitespace-style
   '(face trailing tabs spaces lines-tail newline empty indentation
          space-after-tab space-before-tab))
 '(xref-prompt-for-identifier
   '(not xref-find-definitions xref-find-definitions-other-window
         xref-find-definitions-other-frame xref-find-references)))

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-delete ((t nil)))
 '(diff-hl-insert ((t nil))))
