org-mode

(ttl & "#+title: " p n "#+author: " p n q)
(src & "#+begin_src " p n r n "#+end_src")
(example & "#+begin_example" n r n "#+end_example" n)

python-base-mode

(classmethod "@classmethod" n> "def " p "(cls, " p "):" n> q)
(definit "def __init__(self):" n> q)
(dprint "print(\"" q "\") # DEBUG")
(dprintf "print(f\"{" q "}\")  #  DEBUG")
(ifmain n "if __name__ == \"__main__\":" n> q)
(testcase "class Test" p "(unittest.TestCase):" n> "def test_" p "(self):" n> q)

js-base-mode typescript-ts-base-mode

(dprint "console.log(\"" q "\") // DEBUG")

(if "if (" p ") {" n> q n "}" >)
(el "if (" p ") {" n> p n "} else { " > n> q n "}" >)
(elif "else if (" p ") {" n> q n "}" >)
(else "else {" n> q n "}" >)
(cop "( " p ") ? " p " : " q";")

(while "while (" p ") {" n> q n "}" >)
(dowhile "do {" n> p n> "} while (" q ");")

(for "for (" p "; " p "; " p ") {" n> q n "}" >)
(fin "for (" p " in " p ") {" n> q n "}" >)
(fof "for (const " p " of " p ") {" n> q n "}" >)

(imp "import {" p "} from \"" q "\";")
(impas "import {" p " as " p "} from \"" q "\";")

