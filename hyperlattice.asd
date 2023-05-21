(in-package :cl-user)

(defpackage hyperlattice/asdf
  (:use cl asdf uiop)
  (:export #:hyperlattice/asdf-system))

(in-package :hyperlattice/asdf)

(defsystem hyperlattice
    :description "Lattice, hyperlattice, and probabilistic-hyperlattice datatypes."
    :author "\"the Phoeron\" Colin J.E. Lupton"
    :license "MIT"
    :version "0.1.0"
    :depends-on (alexandria)
    :components ((:file "package")
                 (:file "hash-table-utils")
                 (:file "lattices")
                 (:file "hyperlattices")
                 (:file "probabilistic-hyperlattices")))
