(defpackage :bloki.parser.parser
  (:use :cl
        :bloki.parser.ast
        :bloki.parser.combinators
        :bloki.parser.result)
  (:export
   #:parse))
(in-package :bloki.parser.parser)

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

;; pfuncarg-pair := <symbol> <many-1-whitespace> <atom>
(defun pfuncarg-pair ()
  (seq (lambda (symbol spaces atom)
         (declare (ignore spaces))
         (presult-ok (make-argument-node :name symbol :value atom) (presult-remaining atom)))
       (psymbol)
       (many-1 (whitespace))
       (patom)))

;; NOTE it uses `car' because the arguments are added to the head of the list
;; TODO implement argument-list-node-last-argument
;; funcarg-list := <funcarg-pair>+
(defun pfuncarg-list ()
  (papply (lambda (match input)
            (if (presult-success match)
                (presult-ok match (presult-remaining (car (argument-list-node-arguments match))))
                (presult-fail input)))
          :to (many-1 (pfuncarg-pair) :initial (make-argument-list-node)
                                      :with #'concat-argument)))

;; pfuncarg := <patom>
;;           | <pfuncarg-list>
(defun pfuncarg ()
  (or-else (pfuncarg-list)
           (patom)))

;; pfuncall := <identifier> <many-1-whitespace> <funcarg>
(defun pfuncall ()
  (seq (lambda (identifier spaces args)
         (declare (ignore spaces))
         (presult-ok (make-funcall-node :name identifier
                                        :argument-list (presult-matched args))
                     (presult-remaining args)))
       (pidentifier)
       (many-1 (whitespace))
       (pfuncarg)))

;; pblock-body := <funcall>
;;              | <atom>
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
