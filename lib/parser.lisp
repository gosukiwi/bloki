;; Persers abstract the way input is conumsed. This is a simple wrapper using strings
;; for development purposes. This should use streams.

(defstruct pinput
  (contents nil :type string))

(defun make-pinput-from-char (char)
  (make-pinput :contents (coerce (list char) 'string)))

(defun pinput-char-at (index input)
  (elt (pinput-contents input) index))

(defun pinput-skip (amount input)
  (make-pinput :contents (subseq (pinput-contents input) amount)))

(defun pinput-peek (input)
  "peeks some input, returns the first character and the rest"
  (let ((head (pinput-char-at 0 input))
        (tail (pinput-skip 1 input)))
    (values head tail)))

(defun empty-pinput ()
  (make-pinput :contents ""))

(defun pinput-concat (a b)
  (make-pinput :contents (concatenate 'string (pinput-contents a) (pinput-contents b))))

(defun make-pinput-from-string (str)
  (make-pinput :contents str))



;; Parser result
(defstruct presult
  (success   nil :type boolean)
  (matched   nil :type pinput)
  (remaining nil :type pinput))

;; Utility functions for parsers
(defun presult-ok (matched rest)
  (make-presult :success t :matched matched :remaining rest))

(defun presult-fail (input)
  (make-presult :success nil :matched (empty-pinput) :remaining input))

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
  (apply parser (list input)))

(defun test-parser (parser input)
  "Helper function for playing around with the REPL"
  (apply parser (list (make-pinput-from-string input))))


;; Parsers
;; =======

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

(defun many-1 (parser)
  (lambda (input)
    (let ((matched (presult-fail input))
          (remaining input)
          (iterating t))
      (loop for new-matched = (run-parser parser remaining)
            while iterating
            do (cond
                 ((presult-success new-matched)
                  (setq matched (concat-presult matched new-matched))
                  (setq remaining (presult-remaining new-matched)))
                 (t
                  (setq iterating nil)))
            finally (return matched)))))

(defun many-0 (parser)
  (lambda (input)
    (let ((result (run-parser (many-1 parser) input)))
      (if (presult-success result)
          result
          (presult-ok (empty-pinput) input)))))

;; Binary parser combinators

;; Interface with the outer world
(defun parser (input)
  "Takes bloki input, returns an AST."
  t)
