(in-package :cl-user)
(defpackage :bloki.parser.parser
  (:use :cl
        :bloki.parser.ast
        :bloki.parser.combinators
        :bloki.parser.input.string
        :bloki.parser.result)
  (:export
   #:parse
   #:pblock
   #:test-parser))
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
  (seq (lambda (identifier)
         (presult-ok (make-identifier-node :value (pinput-contents (presult-matched identifier)))
                     (presult-remaining identifier)))
       (many-1 (any-of "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890-_/?+-*/<>|"))))

(defun psymbol ()
  (and-then (pone #\:)
            (pidentifier)))

;; blocks
;; ======

;; pfuncarg-pair := <symbol> <many-1-whitespace> <block>
(defun pfuncarg-pair ()
  (seq (lambda (s1 symbol s2 atom s3)
         (declare (ignore s1))
         (declare (ignore s2))
         (declare (ignore s3))
         (presult-ok (make-argument-node :name symbol :value atom) (presult-remaining atom)))
       (whitespace*)
       (psymbol)
       (whitespace+)
       (pblock)
       (whitespace*)))

;; funcarg-list := <funcarg-pair>+
(defun pfuncarg-list ()
  (many-1 (pfuncarg-pair) :initial (make-argument-list-node)
                          :with #'add-argument
                          :wrap t))

(defun pfuncarg-single ()
  (seq (lambda (matched-block)
         (let ((argument-node (make-argument-node :value matched-block)))
           (presult-ok (make-argument-list-node :arguments (list argument-node))
                       (presult-remaining matched-block))))
       (pblock)))

;; pfuncarg := <patom>
;;           | <pfuncarg-list>
(defun pfuncarg ()
  (or-else (pfuncarg-list)
           (pfuncarg-single)))

;; pfuncall := <block> <many-1-whitespace> <funcarg>
(defun pfuncall ()
  (seq (lambda (fn spaces args)
         (declare (ignore spaces))
         (presult-ok (make-funcall-node :name fn
                                        :argument-list (presult-matched args))
                     (presult-remaining args)))
       (pblock)
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

(defun pparam-list ()
  (many-1 (seq (lambda (identifier optspace)
                 (presult-ok (presult-matched identifier) (presult-remaining optspace)))
               (pidentifier)
               (whitespace*))
          :with #'build-parameter-list-node
          :initial (make-parameter-list-node)
          :wrap t))

;; special forms
;; =============

(defun pparams ()
  (between :lhs   (pone #\[)
           :match (pparam-list)
           :rhs   (pone #\])))

;; pfunc := fn <identifier> <params> <blocks>+
(defun pfunc ()
  (seq ()
       (pstring "fn")
       (whitespace+)
       (pidentifier)
       (whitespace+)
       (pparams)
       (many-1 (pblock))))

;; end of special forms

;; block-body := <binary-funcall>
;;             | <funcall>
(defun pblock-body ()
  (between :lhs   (whitespace*)
           :match (or-else (pfunc)
                           ; end of special forms
                           (pbinary-funcall)
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
