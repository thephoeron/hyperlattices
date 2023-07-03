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

(defclass probabilistic-lattice ()
  ((lattice :accessor lattice :initarg :lattice :type list)
   (probabilities :accessor probabilities :initarg :probabilities :type list)))

(defgeneric probabilistic-lattice-supremum (plattice)
  (:documentation "Returns the supremum of the given probabilistic lattice."))

(defgeneric probabilistic-lattice-infimum (plattice)
  (:documentation "Returns the infimum of the given probabilistic lattice."))

(defgeneric probabilistic-lattice-join (plattice a b)
  (:documentation "Returns the join of the two elements a and b in the given probabilistic lattice."))

(defgeneric probabilistic-lattice-meet (plattice a b)
  (:documentation "Returns the meet of the two elements a and b in the given probabilistic lattice."))

(defgeneric probabilistic-lattice-element-of (plattice x)
  (:documentation "Returns true if the given element x is in the given probabilistic lattice."))

(defgeneric probabilistic-lattice-relation-of (plattice a b)
  (:documentation "Returns the relation between the two elements a and b in the given probabilistic lattice."))

(defgeneric probabilistic-lattice-supremum-of (plattice xs)
  (:documentation "Returns the supremum of the given set of elements in the given probabilistic lattice."))

(defgeneric probabilistic-lattice-infimum-of (plattice xs)
  (:documentation "Returns the infimum of the given set of elements in the given probabilistic lattice."))

(defgeneric probabilistic-lattice-member-p (plattice x)
  (:documentation "Returns true if the given element x is in the given probabilistic lattice."))

(defgeneric probabilistic-lattice-closure (plattice xs)
  (:documentation "Returns the closure of the given set of elements in the given probabilistic lattice."))

(defgeneric probabilistic-lattice-cover (plattice xs)
  (:documentation "Returns the cover of the given set of elements in the given probabilistic lattice."))

(defmethod probabilistic-lattice-supremum ((plattice probabilistic-lattice))
  (reduce #'(lambda (a b) (if (> (nth a (probabilities plattice))
                                 (nth b (probabilities plattice)))
                              a b))
          (lattice plattice)))

(defmethod probabilistic-lattice-infimum ((plattice probabilistic-lattice))
  (reduce #'(lambda (a b) (if (< (nth a (probabilities plattice))
                                 (nth b (probabilities plattice)))
                              a b))
          (lattice plattice)))

(defmethod probabilistic-lattice-join ((plattice probabilistic-lattice) a b)
  (let ((pa (nth a (probabilities plattice)))
        (pb (nth b (probabilities plattice))))
    (if (> pa pb) a b)))

(defmethod probabilistic-lattice-meet ((plattice probabilistic-lattice) a b)
  (let ((pa (nth a (probabilities plattice)))
        (pb (nth b (probabilities plattice))))
    (if (< pa pb) a b)))

(defmethod probabilistic-lattice-element-of ((plattice probabilistic-lattice) x)
  (member x (lattice plattice)))

(defmethod probabilistic-lattice-relation-of ((plattice probabilistic-lattice) a b)
  (let ((pa (nth a (probabilities plattice)))
        (pb (nth b (probabilities plattice))))
    (if (= pa pb) 'incomparable
        (if (> pa pb) 'precedes 'follows))))

(defmethod probabilistic-lattice-supremum-of ((plattice probabilistic-lattice) xs)
  (reduce #'(lambda (a b) (if (> (nth a (probabilities plattice))
                                 (nth b (probabilities plattice)))
                              a b))
          xs))

(defmethod probabilistic-lattice-infimum-of ((plattice probabilistic-lattice) xs)
  (reduce #'(lambda (a b) (if (< (nth a (probabilities plattice))
                                 (nth b (probabilities plattice)))
                              a b))
          xs))

(defmethod probabilistic-lattice-member-p ((plattice probabilistic-lattice) x)
  (member x (lattice plattice)))

(defmethod probabilistic-lattice-closure ((plattice probabilistic-lattice) xs)
  (let ((result xs))
    (dolist (x xs)
      (dolist (y xs)
        (when (eq (probabilistic-lattice-relation-of plattice x y) 'precedes)
          (push y result))))
    (remove-duplicates result)))

(defmethod probabilistic-lattice-cover ((plattice probabilistic-lattice) xs)
  (let ((result xs))
    (dolist (x xs)
      (dolist (y xs)
        (when (eq (probabilistic-lattice-relation-of plattice x y) 'follows)
          (push y result))))
    (remove-duplicates result)))
