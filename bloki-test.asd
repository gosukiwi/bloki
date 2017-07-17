(defsystem "bloki-test"
  :author "Federico Ramirez"
  :licence "MIT"
  :depends-on ("bloki" "prove")
  :defsystem-depends-on ("prove-asdf")
  :pathname "test/"
  :components ((:test-file "parser-tests"))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
