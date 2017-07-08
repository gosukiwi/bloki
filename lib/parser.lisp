;; Parsers need a way to get and consume input
;; Below is a simple wrapper using strings.
;; This is just for development purposes, ideally this would use some kind of stream

(defun make-pinput (char)
  (coerce (list char) 'string))

(defun peek (input)
  "peeks some input, returns the first character and the rest"
  (let ((head (elt input 0))
        (tail (subseq input 1)))
    (values head tail)))

(defun stringify (input)
  (if (stringp input)
      input
      (list input)))

(defun pinput-concat (a b)
  (if a
      (if b
          (concatenate 'string (stringify a) (stringify b))
          a)
      b))

;; Parser result
(defstruct presult
  (success   nil :type boolean)
  (matched   nil :type string)
  (remaining nil :type string))
;; TODO :type should be pinput rather than string

;; Utility functions for parsers
(defun presult-ok (matched rest)
  (make-presult :success t :matched matched :remaining rest))

(defun presult-fail (input)
  (make-presult :success nil :matched "" :remaining input))

;; results look like this: '(t #\f "oo")
;;                         '(nil nil "foo")
(defun presult-concat (a b)
  (if (presult-success b)
      (if (presult-success a)
          (let ((result (pinput-concat (presult-matched a) (presult-matched b))))
            (presult-ok result (presult-remaining b)))
          b)
      a))

(defmacro defp (name args body)
  "Sugar for making parsers. In the parser body you'll have access to the variables:
   head-as-char, head, tail, and input."
  `(defun ,name ,args
     (lambda (input)
       (multiple-value-bind (head-as-char tail) (peek input)
         (let ((head (make-pinput head-as-char)))
           ,body)))))

(defun run-parser (parser input)
  "Helper function to run parsers on a given input"
  (apply parser (list input)))


;; Parsers
;; =======

;; takes a string of possible characters to pick from
(defp any-of (chars)
  (loop named char-loop for char in (coerce chars 'list)
        when (equalp char head-as-char)
          do (return-from char-loop (presult-ok head tail))
        finally (return-from char-loop (presult-fail input))))

(defp one (char)
  (if (equalp char head-as-char)
      (presult-ok head tail)
      (presult-fail input)))

(defp whitespace ()
  (run-parser (any-of '(#\Space #\Backspace #\Linefeed #\Tab #\Return)) input))

;; Unary parser combinators
;; They take a parser and return another parser

;; matches something one or more times
(defun many-1 (parser)
  (lambda (input)
    (let ((matched (presult-fail input))
          (remaining input)
          (iterating t))
      (loop for new-matched = (run-parser parser remaining)
            while iterating
            do (cond
                 ((presult-success new-matched)
                  (setq matched (presult-concat matched new-matched))
                  (setq remaining (presult-remaining new-matched)))
                 (t
                  (setq iterating nil)))
            finally (return matched)))))

;; Binary parser combinators

;; Interface with the outer world
(defun parser (input)
  "Takes bloki input, returns an AST."
  t)
