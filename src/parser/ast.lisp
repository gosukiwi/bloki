(defpackage bloki.parser.ast
  (:use :cl
        :bloki.parser.result
        :bloki.parser.input.string)
  (:export
   #:make-node-from-funcpair))
(in-package :bloki.parser.ast)

;; abstract syntax tree stuff

(defstruct named-argument-node
  (name nil :type string)
  (value nil))

;; these are utility functions so the parser can easily make nodes

(defun presult-as-string (result)
  (pinput-as-string (presult-matched result)))

(defun make-node-from-funcpair (name value)
  (let ((pretty-name (presult-as-string name)))
    (make-named-argument-node :name pretty-name :value value)))
