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
  (or-else (and-then (pone #\-)
                     (ppositive-number))
           (ppositive-number)))

;; atoms
;; =====

(defun pnil ()
  (or-else (and-then (pone #\[)
                     (whitespace*)
                     (pone #\]))
           (pstr "nil")))

;; patom := string | number | array
(defun patom ()
  (or-else (pnumber)
           (pstring)
           (pidentifier)
           (pnil)))

(defun pidentifier ()
  (many-1 (any-of "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890-_/?+-*/<>|")))

(defun psymbol ()
  (and-then (pone #\:)
            (pidentifier)))

;; blocks
;; ======

;; pfuncarg-pair := <symbol> <many-1-whitespace> <atom>
(defun pfuncarg-pair ()
  (seq (lambda (s1 symbol s2 atom s3)
         (declare (ignore s1))
         (declare (ignore s2))
         (declare (ignore s3))
         (presult-ok (make-argument-node :name symbol :value atom) (presult-remaining atom)))
       (whitespace*)
       (psymbol)
       (whitespace+)
       (patom)
       (whitespace*)))

;; funcarg-list := <funcarg-pair>+
(defun pfuncarg-list ()
  (many-1 (pfuncarg-pair) :initial (make-argument-list-node)
                          :with #'add-argument
                          :wrap t))

(defun pfuncarg-single ()
  (seq (lambda (atom)
         (let ((argument-node (make-argument-node :value atom)))
           (presult-ok (make-argument-list-node :arguments (list argument-node)) (presult-remaining atom))))
       (patom)))

;; pfuncarg := <patom>
;;           | <pfuncarg-list>
(defun pfuncarg ()
  (or-else (pfuncarg-list)
           (pfuncarg-single)))

;; pfuncall := <identifier> <many-1-whitespace> <funcarg>
(defun pfuncall ()
  (seq (lambda (identifier spaces args)
         (declare (ignore spaces))
         (presult-ok (make-funcall-node :name identifier
                                        :argument-list (presult-matched args))
                     (presult-remaining args)))
       (pidentifier)
       (whitespace+)
       (pfuncarg)))

(defun pbinary-funcall ()
  (seq (lambda (lhs w1 func w2 rhs)
         (declare (ignore w1))
         (declare (ignore w2))
         (let* ((arg1 (make-argument-node :value lhs))
                (arg2 (make-argument-node :value rhs))
                (arglist (make-argument-list-node :arguments (list arg1 arg2))))
           (presult-ok (make-funcall-node :name func
                                          :argument-list arglist
                                          :binary t)
                       (presult-remaining rhs))))
       (pblock)
       (whitespace+)
       (pidentifier)
       (whitespace+)
       (pblock)))

;; block-body := <binary-funcall>
;;             | <funcall>
(defun pblock-body ()
  (between :lhs   (whitespace*)
           :match (or-else (pbinary-funcall)
                           (pfuncall))
           :rhs   (whitespace*)))

;; block := "[" <block-body> "]"
;;        | <atom>
(defun pblock ()
  (or-else (between :lhs   (pone #\[)
                    ;; we don't want to execute `pblock-body' as it makes an infinite loop
                    ;; so we explicitly defer it wrapping it in a parser
                    :match (lambda (input) (run-parser (pblock-body) input))
                    :rhs   (pone #\]))
           (patom)))

;; program := block+
(defun pprogram ()
  (many-1 (pblock) :with #'cons))

;; Interface with the outer world
(defun parse (input)
  "Takes bloki input, returns an AST."
  input)
