(defpackage bloki.parser.ast
  (:use :cl
        :bloki.parser.result
        :bloki.parser.input.string)
  (:export
   #:argument-list-node-arguments
   #:add-argument
   #:make-argument-list-node
   #:make-argument-node
   #:make-funcall-node))
(in-package :bloki.parser.ast)

;; abstract syntax tree stuff

(defstruct argument-node
  name
  value)

(defstruct argument-list-node
  (arguments nil :type list))

(defstruct funcall-node
  name
  (argument-list nil :type argument-list-node)
  (binary nil :type boolean))

;; a is an arg list
;; b is an arg
(defun add-argument (a b)
  (make-argument-list-node :arguments (cons b (argument-list-node-arguments a))))
