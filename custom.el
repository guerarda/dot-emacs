(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(deft-auto-save-interval 0.0)
 '(deft-use-filename-as-title nil)
 '(diff-font-lock-refine nil)
 '(dired-dwim-target t)
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
 '(magit-git-executable "git")
 '(major-mode-remap-alist '((python-mode . python-ts-mode)))
 '(markdown-command "pandoc")
 '(nlinum-highlight-current-line t)
 '(org-agenda-files '("~/Desktop/org"))
 '(org-babel-load-languages
   '((emacs-lisp . t)
     (awk . t)
     (C . t)
     (shell . t)
     (python \.t)))
 '(org-blank-before-new-entry '((heading . auto) (plain-list-item . auto)))
 '(org-capture-templates
   '(("b" "Add a book entry" entry
      (file+headline "~/Desktop/org/books.org" "2024")
      "** %^{Book title}\12:PROPERTIES:\12:Title:    %\\1\12:Author:   %^{Author}\12:Year:     %^{Year}\12:Started:  %^u\12:Finished:\12:END:" :empty-lines-after 1 :kill-buffer t)
     ("t" "Add a TODO entry" entry
      (file "~/Desktop/org/todo.org")
      "* TODO %^{Title}\12:PROPERTIES:\12:CREATED: %U\12:END:\12%?" :empty-lines-before 1 :empty-lines-after 1 :kill-buffer t :prepend t)
     ("n" "Add a note entry" entry
      (file "~/Desktop/org/notes.org")
      "* %^{Title}\12:PROPERTIES:\12:CREATED: %U\12:END:\12%?" :empty-lines-after 1 :prepend t)))
 '(org-confirm-babel-evaluate nil)
 '(org-fontify-whole-heading-line nil)
 '(org-hide-leading-stars t)
 '(org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
 '(org-journal-date-format "%A, %B %e %Y")
 '(org-journal-dir "~/Desktop/org/")
 '(org-journal-file-format "%Y-%m-%d.org")
 '(org-journal-file-header "#+title: ")
 '(org-journal-file-type 'yearly)
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-targets '((org-agenda-files :maxlevel . 3)))
 '(org-refile-use-outline-path 'file)
 '(org-reverse-note-order t)
 '(org-roam-capture-templates
   '(("d" "default" plain "%?" :target
      (file+head "${slug}.org" "#+title: ${title}\12")
      :unnarrowed t)))
 '(org-src-fontify-natively t)
 '(org-src-preserve-indentation t)
 '(org-src-tab-acts-natively t)
 '(org-todo-keyword-faces
   '(("TODO" . "red")
     ("PROGRESS" . "orange")
     ("WAITING" . "violet")
     ("REVIEW" . "medium purple")
     ("CANCELLED" . "pale green")
     ("DONE" . "dark green")))
 '(org-todo-keywords
   '((sequence "TODO(t)" "PROGRESS(p)" "WAITING(w)" "REVIEW(r)" "|" "CANCELLED(c)" "DONE(d)")))
 '(python-shell-interpreter "python3")
 '(reb-re-syntax 'string)
 '(rg-keymap-prefix "\3r")
 '(sentence-end-double-space nil)
 '(visible-bell nil)
 '(wat-ts-mode-indent-level 4)
 '(whitespace-line-column 80)
 '(whitespace-style
   '(face trailing tabs spaces lines-tail newline empty indentation space-after-tab space-before-tab))
 '(xref-prompt-for-identifier
   '(not xref-find-definitions xref-find-definitions-other-window xref-find-definitions-other-frame xref-find-references))
 '(yas-global-mode t))

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :extend nil :stipple nil :background "#282c34" :foreground "#bbc2cf" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 200 :width normal :foundry "outline" :family "Source Code Pro")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
