#|
  This file is a part of nbody project.
|#

(defsystem "nbody"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:sdl2 :cl-opengl :bordeaux-threads)
  :components ((:module "src"
                :components
                ((:file "nbody"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "nbody-test"))))
