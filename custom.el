(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-color-icons nil)
 '(company-backends
   '(company-lsp company-bbdb company-eclim company-semantic company-clang company-xcode company-cmake company-capf company-files
                 (company-dabbrev-code company-gtags company-etags company-keywords)
                 company-oddmuse company-dabbrev))
 '(company-search-regexp-function 'company-search-flex-regexp)
 '(company-show-numbers t)
 '(compilation-always-kill t)
 '(compilation-scroll-output 'first-error)
 '(diff-font-lock-refine nil)
 '(eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
 '(electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
 '(electric-pair-mode t)
 '(enable-recursive-minibuffers t)
 '(flycheck-display-errors-function 'flycheck-display-error-messages-unless-error-list)
 '(frame-resize-pixelwise t)
 '(git-commit-summary-max-length 72)
 '(ivy-count-format "(%d/%d) ")
 '(ivy-re-builders-alist '((counsel-M-x . ivy--regex-fuzzy) (t . ivy--regex-plus)) t)
 '(lsp-enable-completion-at-point nil)
 '(lsp-enable-links nil)
 '(lsp-enable-snippet nil)
 '(lsp-signature-auto-activate nil)
 '(lsp-signature-doc-lines 5)
 '(lsp-signature-render-documentation nil)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-flycheck-list-position 'right)
 '(lsp-ui-sideline-enable nil)
 '(markdown-command "pandoc")
 '(nlinum-highlight-current-line t)
 '(org-agenda-files '("~/Desktop/work.org"))
 '(org-babel-load-languages
   '((emacs-lisp . t)
     (ledger . t)
     (awk . t)
     (C . t)
     (shell . t)))
 '(org-blank-before-new-entry '((heading) (plain-list-item . auto)))
 '(org-capture-templates
   '(("b" "Add a book entry" entry
      (file+headline "~/Documents/books.org" "2019")
      "** %^{Book title}
  :PROPERTIES:
  :Title:    %\\1
  :Author:   %^{Author}
  :Year:     %^{Year}
  :Started:  %^t
  :Finished:
  :END:" :kill-buffer t)))
 '(org-confirm-babel-evaluate nil)
 '(org-fontify-done-headline nil)
 '(org-fontify-whole-heading-line nil)
 '(org-hide-leading-stars t)
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
   '(dired-narrow dired-subtree htmlize web-mode python-black crux all-the-icons doom-modeline minions cmake-font-lock leuven-theme rmsbolt shackle company-lsp lsp-mode lsp-ui counsel-projectile projectile clang-format flx counsel yaml-mode exec-path-from-shell yasnippet flycheck magit cmake-mode uniquify git-gutter-fringe php-mode modern-cpp-font-lock nlinum use-package solarized-theme smex rainbow-delimiters paredit org-bullets magit-svn ledger-mode irony intero ido-vertical-mode glsl-mode cider))
 '(projectile-globally-ignored-directories
   '(".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" ".ccls-cache"))
 '(projectile-globally-ignored-files '("TAGS"))
 '(projectile-other-file-alist
   '(("cpp" "h" "hpp" "ipp")
     ("ipp" "h" "hpp" "cpp")
     ("hpp" "h" "ipp" "cpp" "cc" "mm")
     ("cxx" "h" "hxx" "ixx")
     ("ixx" "h" "hxx" "cxx")
     ("hxx" "h" "ixx" "cxx")
     ("c" "h")
     ("m" "h")
     ("mm" "h" "hpp")
     ("h" "c" "cc" "cpp" "ipp" "hpp" "cxx" "ixx" "hxx" "m" "mm")
     ("cc" "h" "hh" "hpp")
     ("hh" "cc")
     ("vert" "frag")
     ("frag" "vert")
     (nil "lock" "gpg")
     ("lock" "")
     ("gpg" "")))
 '(projectile-sort-order 'recently-active)
 '(projectile-use-git-grep t)
 '(python-shell-interpreter "python3")
 '(sentence-end-double-space nil)
 '(sgml-basic-offset 4)
 '(shackle-mode t)
 '(shackle-rules
   '((c++-mode :select nil :other nil)
     (compilation-mode)
     (magit-status-mode :select t :same t)))
 '(solarized-distinct-doc-face t)
 '(solarized-use-more-italic nil)
 '(swiper-goto-start-of-match nil)
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
 '(default ((t (:inherit nil :stipple nil :background "#002b36" :foreground "#839496" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "outline" :family "SF Mono"))))
 '(doom-modeline-info ((t (:inherit bold))))
 '(doom-modeline-project-dir ((t (:inherit (font-lock-comment-face bold)))))
 '(git-gutter-fr:modified ((((class color) (background dark)) :foreground "#b58900") (((class color) (background light)) :foreground "#f2804f")))
 '(isearch ((t (:background "#eeee00" :foreground "#002b36" :weight normal))))
 '(ivy-action ((t nil)))
 '(ivy-current-match ((((class color) (background dark)) :background "#268bd2" :foreground "#002b36") (((class color) (background light)) :background "#268bd2" :foreground "#fdf6e3")))
 '(ivy-virtual ((t (:inherit font-lock-doc-face :slant italic :weight normal))))
 '(ledger-font-payee-cleared-face ((t (:foreground "#268bd2" :weight normal))))
 '(magit-hash ((t (:foreground "#dc322f"))))
 '(magit-log-author ((t (:foreground "#6c71c4" :weight bold))))
 '(magit-log-date ((t (:foreground "#859900"))))
 '(nlinum-current-line ((t (:inherit linum :foreground "#93a1a1" :weight bold))))
 '(org-level-1 ((t (:height 1.0 :inherit default :foreground "#cb4b16"))))
 '(org-level-2 ((t (:height 1.0 :inherit default :foreground "#859900"))))
 '(org-level-3 ((t (:height 1.0 :inherit default :foreground "#268bd2"))))
 '(org-level-4 ((t (:height 1.0 :inherit default :foreground "#b58900"))))
 '(swiper-background-match-face-1 ((t (:inherit lazy-highlight))))
 '(swiper-background-match-face-2 ((t (:inherit lazy-highlight))))
 '(swiper-background-match-face-3 ((t (:inherit lazy-highlight))))
 '(swiper-background-match-face-4 ((t (:inherit lazy-highlight))))
 '(swiper-match-face-1 ((t (:inherit isearch :foreground "#002b36" :weight normal))))
 '(swiper-match-face-2 ((t (:inherit isearch :foreground "#002b36" :weight normal))))
 '(swiper-match-face-3 ((t (:inherit isearch :foreground "#002b36" :weight normal))))
 '(swiper-match-face-4 ((t (:inherit isearch :foreground "#002b36" :weight normal)))))
