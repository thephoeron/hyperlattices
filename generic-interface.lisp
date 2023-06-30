(in-package :cl-user)

(defpackage hyperlattices/generic-interface
  (:nicknames hyperlattices/api hl-api)
  (:use cl)
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
           #:congruent-p))

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

(defgeneric slice (generalized-lattice)
  (:documentation ""))

(defgeneric merge (generalized-lattice)
  (:documentation ""))

(defgeneric longest-chain-p (generalized-lattice)
  (:documentation ""))

(defgeneric largest-antichain-p (generalized-lattice)
  (:documentation ""))

(defgeneric homomorphic-p (generalized-lattice)
  (:documentation ""))

(defgeneric isomorphic-p (generalized-lattice)
  (:documentation ""))

(defgeneric congruent-p (generalized-lattice)
  (:documentation ""))
