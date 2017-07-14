(defpackage bloki.parser.ast
  (:use :cl
        :bloki.parser.result
        :bloki.parser.input.string)
  (:export
   #:argument-list-node-arguments
   #:concat-argument
   #:make-argument-list-node
   #:make-argument-node))
(in-package :bloki.parser.ast)

;; abstract syntax tree stuff

(defstruct argument-node
  name
  value)

(defstruct argument-list-node
  (arguments nil :type list))

;; a is an arg list
;; b is an arg
(defun concat-argument (a b)
  (make-argument-list-node :arguments (cons b (argument-list-node-arguments a))))
