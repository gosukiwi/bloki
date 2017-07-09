(defpackage bloki.tests
  (:use :cl :prove :bloki))
(in-package :bloki.tests)

(is "hi" (bloki:parse "hi"))

(finalize)
