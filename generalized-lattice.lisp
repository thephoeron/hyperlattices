(in-package :cl-user)

(defpackage hyperlattices/generalized-lattice
  (:nicknames generalized-lattice)
  (:use c2cl hyperlattices/generic-interface)
  (:export #:generalized-lattice)
  (:documentation "Implementation of the GENERALIZED-LATTICE superclass and dispatching methods. Not really intended for direct use, but exists for when the initial subtype is unknown and model-dependent."))

(in-package :hyperlattices/generalized-lattice)

(defclass generalized-lattice ()
  ((elements :accessor elements :initarg :elements :initform nil)
   (sublattices :accessor sublattices :initarg :sublattices :initform nil))
  (:metaclass funcallable-standard-class)
  (:documentation "Implementation of the GENERALIZED-LATTICE algebraic datatype."))

(defun generalized-lattice (&key elements sublattices)
  "Constructor function for the GENERALIZED-LATTICE algebraic datatype."
  (make-instance 'generalized-lattice :elements elements :sublattices sublattices))

(defmethod supremum ((gl-alpha generalized-lattice))
  (supremum-of gl-alpha))

(defmethod infimum ((gl-alpha generalized-lattice))
  (infimum-of gl-alpha))

(defmethod join ((gl generalized-lattice) &rest gls)
  (let ((sublattices (mapcar #'sublattices (cons gl gls)))
        (elements (mapcar #'elements (cons gl gls))))
    (if (every #'null sublattices)
        (make-instance (class-of gl) :elements (reduce #'union elements))
        (let ((sublattice (apply #'join sublattices)))
          (make-instance (class-of gl) :elements (elements sublattice) :sublattices (list sublattice))))))

(defmethod meet ((gl generalized-lattice) &rest gls)
  (let ((sublattices (mapcar #'sublattices (cons gl gls)))
        (elements (mapcar #'elements (cons gl gls))))
    (if (every #'null sublattices)
        (make-instance (class-of gl) :elements (reduce #'intersection elements))
        (let ((sublattice (apply #'meet sublattices)))
          (make-instance (class-of gl) :elements (elements sublattice) :sublattices (list sublattice))))))

(defmethod element-of ((gl-alpha generalized-lattice) element)
  gl-alpha)

(defmethod relation-of ((gl-alpha generalized-lattice) relation)
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

(defmethod slice ((gl-alpha generalized-lattice) sublattice-index)
  (let ((sublattice (nth sublattice-index (sublattices gl-alpha))))
    (if sublattice
        (let ((slice (make-instance (class-of gl-alpha))))
          (setf (sublattices slice) (list sublattice))
          (setf (elements slice) (elements sublattice))
          (setf (sublattices gl-alpha) (list sublattice))
          slice)
        (error "Sublattice not found."))))

(defmethod merge ((gl generalized-lattice) &rest gls)
  (reduce #'merge gls :from-end t :initial-value gl))

(defmethod longest-chain-p ((gl generalized-lattice) chain)
  (= (length (chain gl)) (length chain)))

(defmethod largest-antichain-p ((gl generalized-lattice) antichain)
  (let ((largest-antichain (antichain gl)))
    (and (equal largest-antichain antichain)
         (>= (length largest-antichain) (length (antichain (closure gl)))))))

(defmethod homomorphic-p ((lhs generalized-lattice) (rhs generalized-lattice))
  (equal (supremum lhs) (supremum rhs)))

(defmethod isomorphic-p ((lhs generalized-lattice) (rhs generalized-lattice))
  (and (homomorphic-p lhs rhs)
       (homomorphic-p rhs lhs)))

(defmethod congruent-p ((lhs generalized-lattice) (rhs generalized-lattice))
  (and (equal (supremum lhs) (supremum rhs))
       (equal (infimum lhs) (infimum rhs))
       (equal (elements lhs) (elements rhs))
       (equal (sublattices lhs) (sublattices rhs))))
