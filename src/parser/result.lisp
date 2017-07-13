(defpackage :bloki.parser.result
  (:use :cl
        :bloki.parser.input.string)
  (:export
   #:presult
   #:presult-fail
   #:presult-failp
   #:presult-matched
   #:presult-ok
   #:presult-remaining
   #:presult-success
   #:concat-presult))
(in-package :bloki.parser.result)

(defstruct presult
  (success   nil :type boolean)
  (matched   nil)
  (remaining nil :type pinput))

(define-condition both-parsers-must-pass (error) ())

(defun presult-ok (matched rest)
  (make-presult :success t :matched matched :remaining rest))

(defun presult-fail (input)
  (make-presult :success nil :matched (make-empty-pinput) :remaining input))

(defun presult-failp (result)
  (equalp nil (presult-success result)))

(defun concat-presult (a b)
  (if (and (presult-success a) (presult-success b))
      (presult-ok (pinput-concat (presult-matched a) (presult-matched b)) (presult-remaining b))
      (error 'both-parsers-must-pass)))
