;; Parsers need a way to get and consume input
;; Below is a simple wrapper using strings.
;; This is just for development purposes, ideally this would use some kind of stream

(defun peek (input)
  "peeks some input, returns the first character and the rest"
  (let ((head (elt input 0))
        (tail (subseq input 1)))
    (values head tail)))

(defun stringify (input)
  (if (stringp input)
      input
      (list input)))

(defun concatenate-input (a b)
  (if a
      (if b
          (concatenate 'string (stringify a) (stringify b))
          a)
      b))

;; Utility functions for parsers
(defun parser-ok (matched rest)
  (list t matched rest))

(defun parser-fail (input)
  (list nil nil input))

(defmacro defp (name args body)
  "Sugar for making parsers. In the parser body you'll have access to the variables: head, tail, and input."
  `(defun ,name ,args
     (lambda (input)
       (multiple-value-bind (head tail) (peek input)
         (progn ,body)))))

(defun run-parser (parser input)
  "Helper function to run parsers on a given input"
  (apply parser (list input)))


;; Parsers
;; =======

;; takes a string of possible characters to pick from
(defp any-of (chars)
  (loop named char-loop for char in (coerce chars 'list)
        finally (return-from char-loop (parser-fail input)) do
          (if (equalp char head)
              (return-from char-loop (parser-ok head tail)))))

(defp one (char)
  (if (equalp char head)
      (parser-ok head tail)
      (parser-fail input)))

(defp whitespace ()
  (run-parser (any-of '(#\Space #\Backspace #\Linefeed #\Tab #\Return)) input))

;; Unary parser combinators
;; They take a parser and return another parser

(defun subseq-input (input index)
  (subseq input index))

;; results look like this: '(t #\f "oo")
;;                         '(nil nil "foo")
(defun concatenate-presults (a b)
  (if (car b)
      (if (car a)
          (let ((result (concatenate-input (nth 1 a) (nth 1 b))))
            (list t result (nth 2 b)))
          b)
      a))

;; matches something one or more times
(defun many-1 (parser)
  (lambda (input)
    (let ((matched (list nil nil input))
          (remaining input)
          (iterating t))
      (loop for new-matched = (run-parser parser remaining)
            while iterating
            do (cond
                 ((car new-matched)
                  (setq matched (concatenate-presults matched new-matched))
                  (setq remaining (nth 2 new-matched)))
                 (t
                  (setq iterating nil)))
            finally (return matched)))))

;; Binary parser combinators

;; Interface with the outer world
(defun parser (input)
  "Takes bloki input, returns an AST."
  t)
