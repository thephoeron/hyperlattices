(in-package :cl-user)

(defpackage hyperlattice/asdf
  (:use cl asdf uiop))

(in-package :hyperlattice/asdf)

(defsystem hyperlattice
    :description "Algebraic lattice, hyperlattice, and probabilistic-hyperlattice datatypes."
    :author "\"the Phoeron\" Colin J.E. Lupton"
    :mailto "thephoeron@protonmail.com"
    :homepage "https://thephoeron.github.io/hyperlattice/"
    :source-control (:git "https://github.com/thephoeron/hyperlattice.git")
    :bug-tracker "https:;//github.com/thephoeron/hyperlattice/issues"
    :license "MIT"
    :version "0.1.2"
    :depends-on (alexandria)
    :serial t
    :components ((:file "hash-table-utils")
                 (:file "lattices")
                 (:file "hyperlattices")
                 (:file "probabilistic-hyperlattices")))
