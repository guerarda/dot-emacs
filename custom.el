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
 '(org-babel-load-languages (quote ((emacs-lisp . t) (ledger . t))))
 '(org-confirm-babel-evaluate nil)
 '(org-todo-keyword-faces (quote (("INPROGRESS" . "#6c71c4"))))
 '(org-todo-keywords (quote ((sequence "TODO" "INPROGRESS" "DONE")))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-hash ((t (:foreground "#dc322f"))))
 '(magit-log-author ((t (:foreground "#6c71c4" :weight bold))))
 '(magit-log-date ((t (:foreground "#859900")))))
