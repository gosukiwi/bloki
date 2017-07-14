(defpackage :bloki.parser.parser
  (:use :cl
        :bloki.parser.ast
        :bloki.parser.result
        :bloki.parser.input.string)
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

;; many-1 can take :with and :initial in order to build custom return values
(defp many-1 (parser &key (with #'concat-presult) initial)
  (let ((matched (or initial (presult-ok (make-empty-pinput) input)))
        (remaining input)
        (matched-any nil))
    (loop for new-matched = (run-parser parser remaining)
          while (presult-success new-matched)
          do (setq matched-any t)
          do (setq matched (apply with (list matched new-matched)))
          do (setq remaining (presult-remaining new-matched))
          finally (if matched-any
                      (return matched)
                      (return (presult-fail input))))))

(defun many-0 (parser)
  "matches a parser 0 or more times"
  (popt (many-1 parser)))


;; Combinators

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

;; Binary combinators

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

;; Blocki-specific parsers

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
;; =======

(defun pscientific ()
  (and-then (any-of "eE")
            (any-of "+-")
            (many-1 (pdigit))))

(defun pdecimal ()
  (and-then (pone #\.)
            (many-1 (pdigit))))

(defun pdecimal-or-scientific ()
  (and-then (pdecimal)
            (popt (pscientific))))

(defun ppositive-number ()
  (and-then (many-1 (pdigit))
            (popt (pdecimal-or-scientific))))

(defun pnumber ()
  (or-else
   (and-then (pone #\-)
             (ppositive-number))
   (ppositive-number)))

;; atoms
;; =====

;; patom := string | number | array
(defun patom ()
  (or-else
   (pnumber)
   (pstring)))

(defun pidentifier ()
  (many-1 (any-of "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-_1234567890")))

(defun psymbol ()
  (and-then (pone #\:)
            (pidentifier)))

;; blocks
;; ======

;; pfuncarg-pair := <symbol> <atom>
(defun pfuncarg-pair ()
  (seq (lambda (symbol spaces atom)
         (declare (ignore spaces))
         (presult-ok (make-argument-node :name symbol :value atom) (presult-remaining atom)))
       (psymbol)
       (many-1 (whitespace))
       (patom)))

(defp papply (callback &key to)
  (apply callback (list (run-parser to input))))

;; NOTE it uses `car' because the arguments are added to the head of the list
;; TODO implement argument-list-node-last-argument
(defun pfuncarg-list ()
  (papply (lambda (match)
            (presult-ok match (presult-remaining (car (argument-list-node-arguments match)))))
          :to (many-1 (pfuncarg-pair) :initial (make-argument-list-node)
                                      :with #'concat-argument)))

;; pfuncarg := <patom> | <pfuncarg-pair>+
(defun pfuncarg ()
  (or-else (patom)
           (pfuncarg-list)))

;; pfuncall := <pidentifier> <pfuncarg>
(defun pfuncall ()
  (seq (lambda (identifier spaces args)
         (declare (ignore spaces))
         (presult-ok (make-funcall-node :name identifier :args args) (presult-remaining args)))
       (pidentifier)
       (many-1 (whitespace))
       (pfuncarg)))

;; pblock-body := <pstring>
(defun pblock-body ()
  (or-else (pfuncall)
           (patom)))

;; pblock := "[" <pblock-body> "]"
(defun pblock ()
  (between :lhs (pone #\[) :rhs (pone #\]) :match (pblock-body)))

;; program := block+
(defun pprogram ()
  (many-1 (pblock) :with #'cons))

;; Interface with the outer world
(defun parse (input)
  "Takes bloki input, returns an AST."
  input)
