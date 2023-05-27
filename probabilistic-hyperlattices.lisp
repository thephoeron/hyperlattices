(in-package :cl-user)

(defpackage probabilistic-hyperlattices
    (:use cl hyperlattice/hash-table-utils)
    (:export #:prob-hyperlattice-sup
             #:prob-hyperlattice-inf
             #:prob-hyperlattice
             #:elements-of
             #:sup-of
             #:inf-of
             #:prob-hyperlattice-add
             #:prob-hyperlattice-remove
             #:prob-hyperlattice-member-p
             #:prob-hyperlattice-sup-set
             #:prob-hyperlattice-inf-set
             #:prob-hyperlattice-closure)
  (:documentation "This package implements probabilistic hyperlattices."))

(in-package :probabilistic-hyperlattices)

;; Define a function to compute the supremum of two probabilistic lattices
(defun prob-hyperlattice-sup (a b)
  (make-instance 'prob-hyperlattice :elements (merge-hash-tables (elements-of a) (elements-of b))
                                     :sup (sup-of a)
                                     :inf (inf-of a)))

;; Define a function to compute the infimum of two probabilistic lattices
(defun prob-hyperlattice-inf (a b)
  (make-instance 'prob-hyperlattice :elements (intersection-hash-tables (elements-of a) (elements-of b))
                                     :sup (sup-of a)
                                     :inf (inf-of a)))

;; Define a class to represent a probabilistic hyperlattice
(defclass prob-hyperlattice ()
  ((elements :initarg :elements :accessor elements-of)
   (sup :initarg :sup :accessor sup-of)
   (inf :initarg :inf :accessor inf-of))
  (:default-initargs :elements (make-hash-table :test #'equal)
                     :sup #'prob-hyperlattice-sup
                     :inf #'prob-hyperlattice-inf))

;; Define a function to add an element with a probability to the probabilistic hyperlattice
(defun prob-hyperlattice-add (prob-hyperlattice element probability)
  (setf (gethash element (elements-of prob-hyperlattice)) probability))

;; Define a function to remove an element from the probabilistic hyperlattice
(defun prob-hyperlattice-remove (prob-hyperlattice element)
  (remhash element (elements-of prob-hyperlattice)))

;; Define a function to check if an element is in the probabilistic hyperlattice
(defun prob-hyperlattice-member-p (prob-hyperlattice element)
  (gethash element (elements-of prob-hyperlattice)))

;; Define a function to compute the supremum of a set of probabilistic lattices
(defun prob-hyperlattice-sup-set (prob-hyperlattice set)
  (let ((result (make-hash-table :test #'equal)))
    (dolist (element (hash-keys (elements-of prob-hyperlattice)))
      (let ((prob (reduce #'* (mapcar (lambda (lattice) (gethash element (elements-of lattice))) set))))
        (when (> prob 0)
          (setf (gethash element result) prob))))
    (make-instance 'prob-hyperlattice :elements result :sup (sup-of prob-hyperlattice) :inf (inf-of prob-hyperlattice))))

;; Define a function to compute the infimum of a set of probabilistic lattices
(defun prob-hyperlattice-inf-set (prob-hyperlattice set)
  (let ((result (make-hash-table :test #'equal)))
    (dolist (element (hash-keys (elements-of prob-hyperlattice)))
      (let ((prob (reduce #'* (mapcar (lambda (lattice) (gethash element (elements-of lattice))) set))))
        (when (> prob 0)
          (setf (gethash element result) prob))))
    (make-instance 'prob-hyperlattice :elements result :sup (sup-of prob-hyperlattice) :inf (inf-of prob-hyperlattice))))

;; Define a function to compute the closure of a set of probabilistic lattices
(defun prob-hyperlattice-closure (prob-hyperlattice set)
  (let ((closure set))
    (loop
      for element being the hash-keys of (elements-of prob-hyperlattice)
      unless (member element closure)
      when (every (lambda (x) (prob-hyperlattice-member-p prob-hyperlattice x)) (cons element closure))
      do (push element closure))
    closure))