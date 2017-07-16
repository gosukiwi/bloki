(in-package :cl-user)
(defpackage bloki-test.parser
  (:use
   :cl
   :prove
   :bloki.parser.parser
   :bloki.parser.result))
(in-package :bloki-test.parser)

(defun it-parses (parser input)
  (ok (presult-success (test-parser parser input)) ))

;; try simple blocks
(it-parses (pblock) "1")
(it-parses (pblock) "\"hi\"")
(it-parses (pblock) "[foo 1]")
(it-parses (pblock) "[1 + 2]")
(it-parses (pblock) "[foo \"hi\"]")
(it-parses (pblock) "[foo :bar \"hi\"]")
(it-parses (pblock) "[foo :bar \"hi\" :baz 2]")
(it-parses (pblock) "[[foo 1] + 2]")
(it-parses (pblock) "[[foo 1] + [foo 1]]")
(it-parses (pblock) "[[foo 1] + [2 - 1]]")

(finalize)
