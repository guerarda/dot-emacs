(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(frame-resize-pixelwise t)
 '(git-commit-summary-max-length 72)
 '(lsp-ui-sideline-enable nil)
 '(nlinum-highlight-current-line t)
 '(org-babel-load-languages '((emacs-lisp . t) (ledger . t) (awk . t) (C . t)))
 '(org-blank-before-new-entry '((heading) (plain-list-item . auto)))
 '(org-capture-templates
   '(("b" "Add a book entry" entry
      (file+headline "~/Documents/books.org" "2018")
      "** %^{Book title}
  :PROPERTIES:
  :Title: %\\1
  :Author:  %^{Author}
  :Year:    %^{Year}
  :Started: %^t
  :Finished:
  :END:" :kill-buffer t)))
 '(org-confirm-babel-evaluate nil)
 '(org-fontify-done-headline t)
 '(org-fontify-whole-heading-line t)
 '(org-src-fontify-natively t)
 '(org-src-preserve-indentation t)
 '(org-src-tab-acts-natively t)
 '(org-todo-keyword-faces
   '(("TODO" . "#dc322f")
     ("PROGRESS" . "#b58900")
     ("WAITING" . "#cb4b16")
     ("REVIEW" . "#6c71c4")
     ("CANCELLED" . "#2aa198")
     ("DONE" . "#2aa198")))
 '(org-todo-keywords
   '((sequence "TODO(t)" "PROGRESS(p)" "WAITING(w)" "REVIEW(r)" "|" "CANCELLED(c)" "DONE(d)")))
 '(package-selected-packages
   '(lsp-ui ccls company-lsp lsp-mode yaml-mode exec-path-from-shell yasnippet flycheck magit cmake-mode uniquify git-gutter-fringe php-mode modern-cpp-font-lock nlinum use-package solarized-theme smex rainbow-delimiters projectile paredit org-bullets magit-svn ledger-mode irony intero ido-vertical-mode glsl-mode cider))
 '(projectile-globally-ignored-directories
   '(".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" ".ccls-cache"))
 '(projectile-globally-ignored-files '("TAGS"))
 '(sgml-basic-offset 4)
 '(visible-bell nil)
 '(whitespace-line-column 80)
 '(whitespace-style
   '(face trailing tabs spaces lines-tail newline empty indentation space-after-tab space-before-tab))
 '(xref-prompt-for-identifier
   '(not xref-find-definitions xref-find-definitions-other-window xref-find-definitions-other-frame xref-find-references))
 '(yas-global-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#002b36" :foreground "#839496" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "SF Mono"))))
 '(git-gutter-fr:modified ((t (:foreground "#b58900" :weight bold))))
 '(magit-hash ((t (:foreground "#dc322f"))))
 '(magit-log-author ((t (:foreground "#6c71c4" :weight bold))))
 '(magit-log-date ((t (:foreground "#859900"))))
 '(nlinum-current-line ((t (:inherit linum :foreground "#93a1a1" :weight bold)))))
