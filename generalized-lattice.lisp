(in-package :cl-user)

(defpackage hyperlattices/generalized-lattice
  (:nicknames generalized-lattice)
  (:use c2cl hyperlattices/generic-interface)
  (:export #:generalized-lattice)
  (:documentation "Implementation of the GENERALIZED-LATTICE superclass and dispatching methods. Not really intended for direct use, but exists for when the initial subtype is unknown and model-dependent."))

(in-package :hyperlattices/generalized-lattice)

(defclass generalized-lattice () ())

(defmethod supremum ((gl-alpha generalized-lattice))
  gl-alpha)

(defmethod infimum ((gl-alpha generalized-lattice))
  gl-alpha)

(defmethod join ((gl-alpha generalized-lattice))
  gl-alpha)

(defmethod meet ((gl-alpha generalized-lattice))
  gl-alpha)

(defmethod element-of ((gl-alpha generalized-lattice))
  gl-alpha)

(defmethod relation-of ((gl-alpha generalized-lattice))
  gl-alpha)

(defmethod supremum-of ((gl-alpha generalized-lattice))
  gl-alpha)

(defmethod infimum-of ((gl-alpha generalized-lattice))
  gl-alpha)

(defmethod member-p ((gl-alpha generalized-lattice))
  gl-alpha)

(defmethod closure ((gl-alpha generalized-lattice))
  gl-alpha)

(defmethod cover ((gl-alpha generalized-lattice))
  gl-alpha)

(defmethod dimension ((gl-alpha generalized-lattice))
  gl-alpha)

(defmethod chain ((gl-alpha generalized-lattice))
  gl-alpha)

(defmethod antichain ((gl-alpha generalized-lattice))
  gl-alpha)

(defmethod slice ((gl-alpha generalized-lattice))
  gl-alpha)

(defmethod merge ((gl-alpha generalized-lattice))
  gl-alpha)

(defmethod longest-chain-p ((gl-alpha generalized-lattice))
  gl-alpha)

(defmethod largest-antichain-p ((gl-alpha generalized-lattice))
  gl-alpha)

(defmethod homomorphic-p ((gl-alpha generalized-lattice))
  gl-alpha)

(defmethod isomorphic-p ((gl-alpha generalized-lattice))
  gl-alpha)

(defmethod congruent-p ((gl-alpha generalized-lattice))
  gl-alpha)
