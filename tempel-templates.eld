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
