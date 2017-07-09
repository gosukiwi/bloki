(defpackage :bloki.parser.parser
  (:use :cl
        :bloki.parser.input.string
        :bloki.parser.result)
  (:export
   #:parse))
(in-package :bloki.parser.parser)

(defmacro defp (name args body)
  "Little sugar for making parsers"
  `(defun ,name ,args
     (lambda (input)
       (progn ,body))))

(defun run-parser (parser input)
  "Run a parser on a given pinput"
  (if (pinput-emptyp input)
      (presult-fail input)
      (apply parser (list input))))

(defun test-parser (parser input)
  "Helper function for playing around with the REPL"
  (run-parser parser (make-pinput-from-string input)))

;; Base Parsers
;; ============
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

(defp whitespace ()
  (run-parser (any-of '(#\Space #\Backspace #\Linefeed #\Tab #\Return)) input))

(defp popt (parser)
  (let ((result (run-parser parser input)))
    (if (presult-success result)
        result
        (presult-ok (make-empty-pinput) input))))

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

(defp many-1 (parser)
  (let ((matched (presult-fail input))
        (remaining input)
        (iterating t))
    (loop for new-matched = (run-parser parser remaining)
          while iterating
          if (presult-success new-matched)
            do (progn
                 (setq matched (concat-presult matched new-matched))
                 (setq remaining (presult-remaining new-matched)))
          else do (setq iterating nil)
          finally (return matched))))

(defun many-0 (parser)
  "matches a parser 0 or more times"
  (popt (many-1 parser)))

;; Binary parser combinators
(defp and-then (p1 p2)
  (let ((result (run-parser p1 input)))
    (if (presult-success result)
        (let ((result2 (run-parser p2 (presult-remaining result))))
          (if (presult-success result2)
              (concat-presult result result2)
              (presult-fail input)))
        result)))

(defp or-else (p1 p2)
  (let ((result (run-parser p1 input)))
    (if (presult-success result)
        result
        (run-parser p2 input))))

(defp and-then-ignore (p1 p2)
  (let ((result (run-parser p1 input)))
    (if (presult-success result)
        (let ((result2 (run-parser p2 (presult-remaining result))))
          (presult-ok (presult-matched result) (presult-remaining result2)))
        result)))

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

;; Blocki parser

;; strings
;; =======
(defun pquote ()
  (pone #\"))

(defun hexdigits ()
  (and-then
   (pone #\u)
   (pexactly 4 (any-of "0123456789abcdef"))))

(defun any-escaped-char ()
  (and-then (pone #\\)
            (or-else (any-of "\"/bfnr")
                     (hexdigits))))

(defun any-unescaped-char ()
  (any-but "\"\\")) ;; anything but a quote (") or a backquote (\)

(defun string-char ()
  (or-else
   (any-unescaped-char)
   (any-escaped-char)))

(defun pstring ()
  (between :lhs (pquote) :rhs (pquote) :match (many-0 (string-char))))

;; numbers

;; program := block+
(defun pprogram ()
  (many-1 (pblock)))

;; Interface with the outer world
(defun parse (input)
  "Takes bloki input, returns an AST."
  input)
