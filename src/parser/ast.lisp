(in-package :cl-user)
(defpackage bloki.parser.ast
  (:use :cl
        :bloki.parser.result
        :bloki.parser.input.string)
  (:export
   #:argument-list-node-arguments
   #:add-argument
   #:make-argument-list-node
   #:make-argument-node
   #:make-funcall-node
   #:make-identifier-node
   #:make-parameter-list-node
   #:build-parameter-list-node
   #:make-function-definition-node))
(in-package :bloki.parser.ast)

;; abstract syntax tree stuff

(defstruct identifier-node
  value)

(defstruct argument-node
  name
  value)

(defstruct argument-list-node
  (arguments nil :type list))

(defstruct funcall-node
  name
  (argument-list nil :type argument-list-node)
  (binary nil :type boolean))

(defstruct parameter-list-node
  (parameters nil :type list))

(defstruct function-definition-node
  name
  params
  body)

;; a is an arg list
;; b is an arg
(defun add-argument (a b)
  (make-argument-list-node :arguments (cons b (argument-list-node-arguments a))))

(defun build-parameter-list-node (a b)
  (make-parameter-list-node :parameters (cons (presult-matched b) (parameter-list-node-parameters a))))
