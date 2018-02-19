#|
  This file is a part of nbody project.
|#

(defsystem "nbody-test"
  :defsystem-depends-on ("prove-asdf")
  :author ""
  :license ""
  :depends-on ("nbody"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "nbody"))))
  :description "Test system for nbody"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
