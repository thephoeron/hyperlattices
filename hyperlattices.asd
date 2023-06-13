(in-package :cl-user)

(defpackage hyperlattices/asdf
  (:use cl asdf uiop))

(in-package :hyperlattices/asdf)

(defsystem hyperlattices
    :description "Generalized Lattice datatypes, incl., lattices, hyperlattices, and probabilistic-hyperlattices."
    :author "\"the Phoeron\" Colin J.E. Lupton"
    :mailto "thephoeron@protonmail.com"
    :homepage "https://thephoeron.github.io/hyperlattices/"
    :source-control (:git "https://github.com/thephoeron/hyperlattices.git")
    :bug-tracker "https:;//github.com/thephoeron/hyperlattices/issues"
    :license "MIT"
    :version (:read-file-form "VERSION")
    :depends-on (alexandria)
    :serial t
    :components ((:file "generic-interface")
                 (:file "hash-table-utils")
                 (:file "generalized-lattices")
                 (:file "lattices")
                 (:file "hyperlattices")
                 (:file "probabilistic-hyperlattices"))
    :in-order-to ((test-op (test-op :hyperlattice/test))))

(defsystem hyperlattices/test
    :description "Test suite for the Hyperlattice library."
    :author "\"the Phoeron\" Colin J.E. Lupton"
    :mailto "thephoeron@protonmail.com"
    :homepage "https://thephoeron.github.io/hyperlattices/"
    :source-control (:git "https://github.com/thephoeron/hyperlattices.git")
    :bug-tracker "https:;//github.com/thephoeron/hyperlattices/issues"
    :license "MIT"
    :version (:read-file-form "VERSION")
    :depends-on (hyperlattice parachute)
    :serial t
    :components ((:module "test"
                  :serial t
                  :components ((:file "hyperlattice-tests"))))
    :perform (test-op (op c) (uiop:symbol-call :parachute :test :hyperlattice/test)))