(defpackage :bloki.parser.combinators
  (:use :cl
        :bloki.parser.result
        :bloki.parser.input.string)
  (:export
   #:and-then
   #:any-but
   #:any-of
   #:between
   #:defp
   #:many-0
   #:many-1
   #:nothing
   #:or-else
   #:papply
   #:pdigit
   #:pexactly
   #:pignore
   #:pone
   #:popt
   #:pstr
   #:run-parser
   #:seq
   #:test-parser
   #:whitespace))
(in-package :bloki.parser.combinators)

(defmacro defp (name args body)
  "Little sugar for making parsers"
  `(defun ,name ,args
     (lambda (input)
       (progn ,body))))

(defun run-parser (parser input)
  "Run a parser on a given pinput"
  (apply parser (list input)))

(defun test-parser (parser input)
  "Helper function for playing around with the REPL"
  (run-parser parser (make-pinput-from-string input)))

;; Base Parsers
;; ============
(defp nothing ()
  (presult-ok nil input))

(defp any-of (chars)
  (multiple-value-bind (head-as-char tail) (pinput-peek input)
    (let ((head (make-pinput-from-char head-as-char)))
      (loop for char in (coerce chars 'list)
            when (equalp char head-as-char)
              do (return (presult-ok head tail))
            finally (return (presult-fail input))))))

(defp any-but (chars)
  (multiple-value-bind (head-as-char tail) (pinput-peek input)
    (let ((head (make-pinput-from-char head-as-char)))
      (loop for char in (coerce chars 'list)
            when (equalp char head-as-char)
              do (return (presult-fail input))
            finally (return (presult-ok head tail))))))

(defp pone (char)
  (multiple-value-bind (head-as-char tail) (pinput-peek input)
    (let ((head (make-pinput-from-char head-as-char)))
      (if (equalp char head-as-char)
          (presult-ok head tail)
          (presult-fail input)))))

(defp pstr (str)
  (multiple-value-bind (success rest) (pinput-grab str input)
    (if success
        (presult-ok (make-pinput-from-string str) rest)
        (presult-fail input))))

(defun whitespace ()
  (any-of '(#\Space #\Backspace #\Linefeed #\Tab #\Return)))

(defun pdigit ()
  (any-of "0123456789"))

;; Combinators

;; match something maybe
(defp popt (parser)
  (let ((result (run-parser parser input)))
    (if (presult-success result)
        result
        (presult-ok (make-empty-pinput) (presult-remaining result)))))

;; match something optionally, and ignore it in the result
(defp pignore (parser)
  (presult-ok (make-empty-pinput) (presult-remaining (run-parser parser input))))

;; matches a parser exactly n times
(defp pexactly (times parser)
  (let ((matched (presult-fail input))
        (remaining input))
    (loop for n from 1 to times
          for new-matched = (run-parser parser remaining)
          when (presult-failp new-matched)
            do (return (presult-fail input))
          do (setq remaining (presult-remaining new-matched))
          do (setq matched (concat-presult matched new-matched))
          finally (return matched))))

;; many-1 matches something 1 or more times
;; use :with :initial and :wrap to return custom values
(defp many-1 (parser &key (with #'concat-presult) initial wrap)
  (let ((matched (or initial (presult-ok (make-empty-pinput) input)))
        (remaining input)
        (matched-any nil))
    (loop for new-matched = (run-parser parser remaining)
          while (presult-success new-matched)
          do (setq matched-any t)
          do (setq matched (apply with (list matched new-matched)))
          do (setq remaining (presult-remaining new-matched))
          finally (if matched-any
                      (return (if wrap
                                  (presult-ok matched remaining)
                                  matched))
                      (return (presult-fail input))))))

(defun many-0 (parser)
  "matches a parser 0 or more times"
  (popt (many-1 parser)))

;; this function evaluates the parsers in the given order.
;; it saves the results and passes them as arguments to a given function
(defp seq (callback &rest parsers)
  (let ((remaining input)
        (collected nil)
        (matched-amount 0))
    (setq collected (loop for p in parsers
                          for result = (run-parser p remaining)
                          while (presult-success result)
                          do (setq remaining (presult-remaining result))
                          do (incf matched-amount)
                          collect result))
    (if (= (length parsers) matched-amount)
        (apply callback collected)
        (presult-fail input))))

(defun and-then (&rest parsers)
  (apply #'seq (cons (lambda (&rest results) (reduce #'concat-presult results)) parsers)))

(defp or-else (&rest parsers)
  (loop for p in parsers
        for result = (run-parser p input)
        when (presult-success result)
          do (return result)
        finally (return (presult-fail input))))

(defp between (&key match lhs rhs)
  (let ((lhs-result (run-parser lhs input)))
    (if (presult-success lhs-result)
        (let ((match-result (run-parser match (presult-remaining lhs-result))))
          (if (presult-success match-result)
              (let ((rhs-result (run-parser rhs (presult-remaining match-result))))
                (if (presult-success rhs-result)
                    (presult-ok (presult-matched match-result) (presult-remaining rhs-result))
                    (presult-fail input)))
              (presult-fail input)))
        (presult-fail input))))
