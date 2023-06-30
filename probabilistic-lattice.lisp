(in-package :cl-user)

(defpackage hyperlattices/probabilistic-lattice
  (:nicknames probabilistic-lattice)
  (:use c2cl
        hyperlattices/hash-table-utils
        hyperlattices/generic-interface
        hyperlattices/generalized-lattice)
  (:export)
  (:documentation "Implementation of PROBABILISTIC-LATTICE algebraic datatype's type class and method specializations."))

(in-package :hyperlattices/probabilistic-lattice)
