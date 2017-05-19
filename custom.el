(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-commit-summary-max-length 50)
 '(frame-resize-pixelwise t)
 '(org-babel-load-languages (quote ((emacs-lisp . t) (ledger . t) (awk . t) (C . t))))
 '(org-blank-before-new-entry (quote ((heading) (plain-list-item . auto))))
 '(org-confirm-babel-evaluate nil)
 '(org-cycle-separator-lines 1)
 '(org-fontify-done-headline t)
 '(org-fontify-whole-heading-line t)
 '(org-src-fontify-natively t)
 '(org-src-preserve-indentation t)
 '(org-src-tab-acts-natively t)
 '(org-todo-keyword-faces
   (quote
    (("TODO" . "#dc322f")
     ("PROGRESS" . "#b58900")
     ("WAITING" . "#cb4b16")
     ("REVIEW" . "#6c71c4")
     ("CANCELLED" . "#2aa198")
     ("DONE" . "#2aa198"))))
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t)" "PROGRESS(p)" "WAITING(w)" "REVIEW(r)" "|" "CANCELLED(c)" "DONE(d)"))))
 '(package-selected-packages
   (quote
    (git-gutter-fringe php-mode modern-cpp-font-lock nlinum use-package solarized-theme smex rainbow-delimiters projectile paredit org-bullets magit-svn ledger-mode irony intero ido-vertical-mode glsl-mode cider better-defaults)))
 '(visible-bell nil)
 '(whitespace-style
   (quote
    (face trailing tabs spaces lines-tail newline empty indentation space-after-tab space-before-tab))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-hash ((t (:foreground "#dc322f"))))
 '(magit-log-author ((t (:foreground "#6c71c4" :weight bold))))
 '(magit-log-date ((t (:foreground "#859900")))))
