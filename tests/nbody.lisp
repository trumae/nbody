(defpackage nbody-test
  (:use :cl
        :nbody
        :prove))
(in-package :nbody-test)

;; NOTE: To run this test file, execute `(asdf:test-system :nbody)' in your Lisp.

(setf *enable-colors* nil)

(plan 1)

(subtest "start"
  (is (start 1000) nil)
  (sdl2:push-event :quit))

(finalize)
