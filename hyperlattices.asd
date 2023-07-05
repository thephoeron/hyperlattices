(in-package :cl-user)

(defpackage hyperlattices/asdf
  (:nicknames hyperlattices/sys)
  (:use cl asdf uiop))

(in-package :hyperlattices/asdf)

(defsystem hyperlattices
  :description "Generalized Lattice algebraic datatypes, incl., LATTICE, HYPERLATTICE, PROBABILISTIC-LATTICE, and PROBABILISTIC-HYPERLATTICE."
  :author "\"the Phoeron\" Colin J.E. Lupton"
  :mailto "thephoeron@protonmail.com"
  :homepage "https://thephoeron.github.io/hyperlattices/"
  :source-control (:git "https://github.com/thephoeron/hyperlattices.git")
  :bug-tracker "https://github.com/thephoeron/hyperlattices/issues"
  :license "MIT"
  :version (:read-file-form "VERSION")
  :depends-on (alexandria
               serapeum
               closer-mop
               trivial-types
               baphomet)
  :serial t
  :components ((:file "generic-interface")
               (:file "hash-table-utils")
               (:file "generalized-lattice")
               (:file "lattice")
               (:file "probabilistic-lattice")
               (:file "hyperlattice")
               (:file "probabilistic-hyperlattice"))
  :in-order-to ((test-op (test-op :hyperlattices/test))))

(defsystem hyperlattices/test
  :description "PARACHUTE-powered test suite for the HYPERLATTICES library."
  :author "\"the Phoeron\" Colin J.E. Lupton"
  :mailto "thephoeron@protonmail.com"
  :homepage "https://thephoeron.github.io/hyperlattices/"
  :source-control (:git "https://github.com/thephoeron/hyperlattices.git")
  :bug-tracker "https://github.com/thephoeron/hyperlattices/issues"
  :license "MIT"
  :version (:read-file-form "VERSION")
  :depends-on (hyperlattices
               parachute)
  :serial t
  :components ((:module "test"
                :serial t
                :components ((:file "hyperlattice-tests"))))
  :perform (test-op (op c) (uiop:symbol-call :parachute :test :hyperlattices/test)))
