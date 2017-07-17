(defsystem "bloki"
  :name "bloki"
  :version "0.0.0"
  :author "Federico Ramirez"
  :licence "MIT"
  :description "programming language"
  :long-description "A minimal, simple, functional, dynamic programming language with lots of blocks to play with"
  :pathname "src/"
  :serial t
  :components ((:module "parser"
                :pathname "parser"
                :components
                        ((:file "input-string")
                         (:file "result")
                         (:file "combinators")
                         (:file "ast")
                         (:file "parser"))))
  :in-order-to ((test-op (test-op "bloki-test"))))
