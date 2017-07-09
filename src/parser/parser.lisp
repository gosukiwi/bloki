(defpackage :bloki
  (:use :cl
        :bloki.parser.input.string)
  (:export
   #:parse
   #:test-parser
   #:presult-success))
(in-package :bloki)

(defstruct presult
  (success   nil :type boolean)
  (matched   nil :type pinput)
  (remaining nil :type pinput))

(defun presult-ok (matched rest)
  (make-presult :success t :matched matched :remaining rest))

(defun presult-fail (input)
  (make-presult :success nil :matched (make-empty-pinput) :remaining input))

(defun presult-failp (result)
  (equalp nil (presult-success result)))

(defun concat-presult (a b)
  (if (presult-success b)
      (if (presult-success a)
          (presult-ok (pinput-concat (presult-matched a) (presult-matched b)) (presult-remaining b))
          b)
      a))

(defmacro defp (name args body)
  "Sugar for making parsers. In the parser body you'll have access to the variables:
   head-as-char, head, tail, and input. `head', `tail', and `input' are pinput."
  `(defun ,name ,args
     (lambda (input)
       (multiple-value-bind (head-as-char tail) (pinput-peek input)
         (let ((head (make-pinput-from-char head-as-char)))
           ,body)))))

(defun run-parser (parser input)
  "Run a parser on a given pinput"
  (if (pinput-emptyp input)
      (presult-fail input)
      (apply parser (list input))))

(defun test-parser (parser input)
  "Helper function for playing around with the REPL"
  (apply parser (list (make-pinput-from-string input))))


;; Base Parsers
;; ============
(defp any-of (chars)
  (loop named char-loop for char in (coerce chars 'list)
        when (equalp char head-as-char)
          do (return-from char-loop (presult-ok head tail))
        finally (return-from char-loop (presult-fail input))))

(defp any-but (chars)
  (loop for char in (coerce chars 'list)
        when (equalp char head-as-char)
          do (return (presult-fail input))
        finally (return (presult-ok head tail))))

(defp pone (char)
  (if (equalp char head-as-char)
      (presult-ok head tail)
      (presult-fail input)))

(defun pstr (str)
  (lambda (input)
    (multiple-value-bind (success rest) (pinput-grab str input)
      (if success
          (presult-ok (make-pinput-from-string str) rest)
          (presult-fail input)))))

(defun whitespace ()
  (lambda (input)
    (run-parser (any-of '(#\Space #\Backspace #\Linefeed #\Tab #\Return)) input)))

(defun popt (parser)
  "tries a parser, and it returns true if it doesn't match"
  (lambda (input)
    (let ((result (run-parser parser input)))
      (if (presult-success result)
          result
          (presult-ok (make-empty-pinput) input)))))

(defun pexactly (times parser)
  "matches a parser exactly n times"
  (lambda (input)
    (let ((matched (presult-fail input))
          (remaining input))
      (loop for n from 1 to times
            for new-matched = (run-parser parser remaining)
            when (presult-failp new-matched)
              do (return (presult-fail input))
            do (setq remaining (presult-remaining new-matched))
            do (setq matched (concat-presult matched new-matched))
            finally (return matched)))))

(defun many-1 (parser)
  "matches a parser 1 or more times"
  (lambda (input)
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
            finally (return matched)))))

(defun many-0 (parser)
  "matches a parser 0 or more times"
  (popt (many-1 parser)))

;; Binary parser combinators
(defun and-then (p1 p2)
  (lambda (input)
    (let ((result (run-parser p1 input)))
      (if (presult-success result)
          (let ((result2 (run-parser p2 (presult-remaining result))))
            (if (presult-success result2)
            (concat-presult result result2)
            (presult-fail input)))
          result))))

(defun or-else (p1 p2)
  (lambda (input)
    (let ((result (run-parser p1 input)))
      (if (presult-success result)
          result
          (run-parser p2 input)))))

(defun and-then-ignore (p1 p2)
  "runs the first parser, the second is optional and ignored, yet consumed"
  (lambda (input)
    (let ((result (run-parser p1 input)))
      (if (presult-success result)
          (let ((result2 (run-parser p2 (presult-remaining result))))
            (presult-ok (presult-matched result) (presult-remaining result2)))
          result))))

(defun between (&key match lhs rhs)
  "matches something in between parsers. `(between :lhs p1 :match p1 :rhs p3)'"
  (lambda (input)
    (let ((lhs-result (run-parser lhs input)))
      (if (presult-success lhs-result)
          (let ((match-result (run-parser match (presult-remaining lhs-result))))
            (if (presult-success match-result)
                (let ((rhs-result (run-parser rhs (presult-remaining match-result))))
                  (if (presult-success rhs-result)
                      (presult-ok (presult-matched match-result) (presult-remaining rhs-result))
                      (presult-fail input)))
                (presult-fail input)))
          (presult-fail input)))))

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
