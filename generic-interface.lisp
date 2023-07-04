(in-package :cl-user)

(defpackage hyperlattices/generic-interface
  (:nicknames hyperlattices/api hl/api hl-api)
  (:use c2cl)
  (:export #:supremum
           #:infimum
           #:join
           #:meet
           #:element-of
           #:relation-of
           #:supremum-of
           #:infimum-of
           #:member-p
           #:closure
           #:cover
           #:dimension
           #:chain
           #:antichain
           #:slice
           #:merge
           #:longest-chain-p
           #:largest-antichain-p
           #:homomorphic-p
           #:isomorphic-p
           #:congruent-p)
  (:documentation "This package defines the generic interface for all Generalized Lattice algebraic datatypes."))

(in-package :hyperlattices/api)

(defgeneric supremum (generalized-lattice)
  (:documentation ""))

(defgeneric infimum (generalized-lattice)
  (:documentation ""))

(defgeneric join (generalized-lattice)
  (:documentation ""))

(defgeneric meet (generalized-lattice)
  (:documentation ""))

(defgeneric element-of (generalized-lattice)
  (:documentation ""))

(defgeneric relation-of (generalized-lattice)
  (:documentation ""))

(defgeneric supremum-of (generalized-lattice)
  (:documentation ""))

(defgeneric infimum-of (generalized-lattice)
  (:documentation ""))

(defgeneric member-p (generalized-lattice)
  (:documentation ""))

(defgeneric closure (generalized-lattice)
  (:documentation ""))

(defgeneric cover (generalized-lattice)
  (:documentation ""))

(defgeneric dimension (generalized-lattice)
  (:documentation ""))

(defgeneric chain (generalized-lattice)
  (:documentation ""))

(defgeneric antichain (generalized-lattice)
  (:documentation ""))

(defgeneric slice (generalized-lattice sublattice)
  (:documentation ""))

(defgeneric merge (generalized-lattice &rest generalized-lattices)
  (:documentation ""))

(defgeneric longest-chain-p (generalized-lattice chain)
  (:documentation ""))

(defgeneric largest-antichain-p (generalized-lattice chain)
  (:documentation ""))

(defgeneric homomorphic-p (lhs rhs)
  (:documentation ""))

(defgeneric isomorphic-p (lhs rhs)
  (:documentation ""))

(defgeneric congruent-p (lhs rhs)
  (:documentation ""))
