(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ledger-reports
   (quote
    (("stats" "ledger stats")
     ("bal" "%(ledger) -f %(ledger-file) bal")
     ("reg" "%(ledger) -f %(ledger-file) reg")
     ("payee" "%(ledger) -f %(ledger-file) reg @%(payee)")
     ("account" "%(ledger) -f %(ledger-file) reg %(account)"))))
 '(org-babel-load-languages (quote ((emacs-lisp . t) (ledger . t) (awk . t) (C . t))))
 '(org-confirm-babel-evaluate nil)
 '(org-cycle-separator-lines 1)
 '(org-src-fontify-natively t)
 '(org-src-preserve-indentation t)
 '(org-src-tab-acts-natively t)
 '(org-todo-keyword-faces
   (quote
    (("TODO" . "#dc322f")
     ("INPROGRESS" . "#b58900")
     ("WAITING" . "#cb4b16")
     ("REVIEW" . "#6c71c4")
     ("CANCELLED" . "#2aa198")
     ("DONE" . "#2aa198"))))
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t)" "INPROGRESS(p)" "WAITING(w)" "REVIEW(r)" "|" "CANCELLED(c)" "DONE(d)"))))
 '(package-selected-packages
   (quote
    (modern-cpp-font-lock nlinum use-package solarized-theme smex rainbow-delimiters projectile paredit org-bullets magit-svn ledger-mode irony intero ido-vertical-mode gnuplot glsl-mode cider better-defaults)))
 '(visible-bell nil)
 '(whitespace-style
   (quote
    (face tabs spaces trailing lines space-before-tab newline indentation empty space-after-tab))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-hash ((t (:foreground "#dc322f"))))
 '(magit-log-author ((t (:foreground "#6c71c4" :weight bold))))
 '(magit-log-date ((t (:foreground "#859900")))))
