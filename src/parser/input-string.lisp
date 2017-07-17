;; All parsers must take input somehow. All parser input interfaces live in `src/parser/input'.
;; This is a simple interface around strings. It's used for easy development.
;; TODO Implement an interface around file reading? Wait until it's needed though.
(in-package :cl-user)
(defpackage bloki.parser.input.string
  (:use :common-lisp)
  (:export
   #:make-empty-pinput
   #:make-pinput-from-char
   #:make-pinput-from-string
   #:pinput
   #:pinput-as-string
   #:pinput-concat
   #:pinput-grab
   #:pinput-peek))
(in-package :bloki.parser.input.string)

(defstruct pinput
  (contents nil :type string))

(defun make-pinput-from-char (char)
  (if char
      (make-pinput :contents (coerce (list char) 'string))
      (make-empty-pinput)))

(defun make-pinput-from-string (str)
  (make-pinput :contents str))

(defun make-empty-pinput ()
  (make-pinput :contents ""))

(defun pinput-char-at (index input)
  (elt (pinput-contents input) index))

(defun pinput-skip (amount input)
  (make-pinput :contents (subseq (pinput-contents input) amount)))

(defun pinput-peek (input)
  "peeks some input, returns the first character and the rest"
    (if (pinput-emptyp input)
        (values nil "")
        (let ((head (pinput-char-at 0 input))
              (tail (pinput-skip 1 input)))
          (values head tail))))

(defun pinput-concat (a b)
  (make-pinput :contents (concatenate 'string (pinput-contents a) (pinput-contents b))))

(defun pinput-first (input amount)
  (let ((str (pinput-contents input)))
    (subseq str 0 (min amount (length str)))))

(defun pinput-starts-with (str input)
  (equalp str (pinput-first input (length str))))

(defun pinput-grab (str input)
  "takes a string, tries to grab it from the first of the input.
  if the input starts with the given string, it returns '(t rest)
  otherwise it returns '(nil input)."
  (let ((result (pinput-starts-with str input)))
    (if result
        (values t (pinput-skip (length str) input))
        (values nil input))))

(defun pinput-emptyp (input)
  (= 0 (length (pinput-contents input))))

(defun pinput-as-string (input)
  (pinput-contents input))
