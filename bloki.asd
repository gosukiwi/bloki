(defsystem "bloki"
  :name "bloki"
  :version "0.0.0"
  :maintainer "Federico Ramirez"
  :author "Federico Ramirez"
  :licence "MIT"
  :description "programming language"
  :long-description "A minimal, simple, functional, dynamic programming language with lots of blocks to play with"
  :serial t
  :pathname "src/"
  :components ((:module "parser"
                :pathname "parser"
                :components
                        ((:module "input"
                          :pathname "input"
                          :components
                          ((:file "string")))
                         (:file "result")
                         (:file "ast")
                         (:file "parser"))))
  :in-order-to ((test-op (test-op "bloki/tests"))))

(defsystem "bloki.tests"
  :depends-on ("bloki" "prove")
  :defsystem-depends-on ("prove-asdf")
  :pathname "test/"
  :components ((:test-file "parser-tests"))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
