python-base-mode

(classmethod "@classmethod" n> "def " p "(cls, " p "):" n> q)
(definit "def __init__(self):" n> q)
(ifmain "if __name__ == \"__main__\":" n> q)
(testcase "class Test" p "(unittest.TestCase):" n> "def test_" p "(self):" n> q)
(dprint "print(\"" q "\") # DEBUG")
