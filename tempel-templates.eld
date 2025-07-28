
;;  - A string: It is sent to the hooks in `tempo-insert-string-functions',
;;    and the result is inserted.
;;  - The symbol `p': This position is saved in `tempo-marks'.
;;  - The symbol `r': If `tempo-insert' is called with ON-REGION non-nil
;;    the current region is placed here.  Otherwise it works like `p'.
;;  - (p PROMPT <NAME> <NOINSERT>): If `tempo-interactive' is non-nil, the
;;    user is prompted in the minibuffer with PROMPT for a string to be
;;    inserted.  If the optional parameter NAME is non-nil, the text is
;;    saved for later insertion with the `s' tag.  If there already is
;;    something saved under NAME that value is used instead and no
;;    prompting is made.  If NOINSERT is provided and non-nil, nothing is
;;    inserted, but text is still saved when a NAME is provided.  For
;;    clarity, the symbol `noinsert' should be used as argument.
;;  - (P PROMPT <NAME> <NOINSERT>): Works just like the previous tag, but
;;    forces `tempo-interactive' to be true.
;;  - (r PROMPT <NAME> <NOINSERT>): Like the previous tag, but if
;;    `tempo-interactive' is nil and `tempo-insert' is called with
;;    ON-REGION non-nil, the current region is placed here.  This usually
;;    happens when you call the template function with a prefix argument.
;;  - (s NAME): Inserts text previously read with the (p ..) construct.
;;    Finds the insertion saved under NAME and inserts it.  Acts like `p'
;;    if `tempo-interactive' is nil.
;;  - `&': If there is only whitespace between the line start and point,
;;    nothing happens.  Otherwise a newline is inserted.
;;  - `%': If there is only whitespace between point and end of line,
;;    nothing happens.  Otherwise a newline is inserted.
;;  - `n': Inserts a newline.
;;  - `>': The line is indented using `indent-according-to-mode'.  Note
;;    that you often should place this item after the text you want on
;;    the line.
;;  - `r>': Like `r', but it also indents the region.
;;  - (r> PROMPT <NAME> <NOINSERT>): Like (r ...), but is also indents
;;    the region.
;;  - `n>': Inserts a newline and indents line.
;;  - `o': Like `%' but leaves the point before the newline.
;;  - nil: It is ignored.
;;  - Anything else: Each function in `tempo-user-element-functions' is called
;;    with it as argument until one of them returns non-nil, and the
;;    result is inserted.  If all of them return nil, it is evaluated and
;;    the result is treated as an element to be inserted.  One additional
;;    tag is useful for these cases.  If an expression returns a list (l
;;    foo bar), the elements after `l' will be inserted according to the
;;    usual rules.  This makes it possible to return several elements
;;    from one expression."

org-mode

(ttl & "#+title: " p n "#+author: " p n q)
(src & "#+begin_src " p n r n "#+end_src")
(nsrc & "#+name: " p n "#+begin_src " p n r n "#+end_src")
(example & "#+begin_example" p n r n "#+end_example" n)
(startup & "#+startup: " p n)
(quote & "#+begin_quote" n r n "#+end_quote" n q)

python-base-mode

(classmethod "@classmethod" n> "def " p "(cls, " p "):" n> q)
(definit "def __init__(self):" n> q)
(doc "\"\"\"" q "\"\"\"")
(dprint "print(\"" q "\") # DEBUG")
(dprintf "print(f\"{" q "}\")  #  DEBUG")
(ifmain n "if __name__ == \"__main__\":" n> q)
(testcase "class Test" p "(unittest.TestCase):" n> "def test_" p "(self):" n> q)
(try "try:" n> "pass" q n> "except:" n> "pass" n)
(tryf "try:" n> "pass" q n n> "except:" n> "pass" n n> "finally:" n> "pass" n n)


prog-mode

(fixme (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "FIXME ")
(todo (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO ")
(bug (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "BUG ")
(hack (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "HACK ")

js-base-mode typescript-ts-base-mode

(dprint "console.log(`" q "`) // DEBUG")

(if "if (" p ") {" n> q n "}" >)
(el "if (" p ") {" n> p n "} else { " > n> q n "}" >)
(elif "else if (" p ") {" n> q n "}" >)
(else "else {" n> q n "}" >)
(cop "( " p ") ? " p " : " q";")

(afn "(" p ") => " q)
(aafn "(" p ") => {" n> r> n> "}")
(fnc p "(" q ");")

(while "while (" p ") {" n> q n "}" >)
(dowhile "do {" n> p n> "} while (" q ");")

(for "for (" p "; " p "; " p ") {" n> q n "}" >)
(fin "for (" p " in " p ") {" n> q n "}" >)
(fof "for (const " p " of " p ") {" n> q n "}" >)

(imp "import {" p "} from \"" q "\";")
(impas "import {" p " as " p "} from \"" q "\";")

