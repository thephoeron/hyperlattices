(in-package :cl-user)

(defpackage lattices
  (:use cl hyperlattice/hash-table-utils)
  (:export #:lattice-sup
           #:lattice-inf
           #:lattice
           #:lattice-add
           #:lattice-remove
           #:lattice-member-p
           #:lattice-sup-set
           #:lattice-inf-set
           #:lattice-closure
           #:elements-of
           #:sup-of
           #:inf-of)
  (:documentation "A package implementing the algebraic lattice datatype."))

(in-package :lattices)

;; Define a function to compute the supremum of two elements
(defun lattice-sup (a b)
  (if (eq a b)
      a
      (if (eq a 'bottom)
          b
          (if (eq b 'bottom)
              a
              'top))))

;; Define a function to compute the infimum of two elements
(defun lattice-inf (a b)
  (if (eq a b)
      a
      (if (eq a 'top)
          b
          (if (eq b 'top)
              a
              'bottom))))

;; Define a class to represent a lattice
(defclass lattice ()
  ((elements :initarg :elements :accessor elements-of)
   (sup :initarg :sup :accessor sup-of)
   (inf :initarg :inf :accessor inf-of))
  (:default-initargs :elements (make-hash-table)
                     :sup #'lattice-sup
                     :inf #'lattice-inf))

;; Define a function to add an element to the lattice
(defun lattice-add (lattice element)
  (setf (gethash element (elements-of lattice)) t))

;; Define a function to remove an element from the lattice
(defun lattice-remove (lattice element)
  (remhash element (elements-of lattice)))

;; Define a function to check if an element is in the lattice
(defun lattice-member-p (lattice element)
  (gethash element (elements-of lattice)))

;; Define a function to compute the supremum of a set of elements
(defun lattice-sup-set (lattice set)
  (reduce (sup-of lattice) set))

;; Define a function to compute the infimum of a set of elements
(defun lattice-inf-set (lattice set)
  (reduce (inf-of lattice) set))

;; Define a function to compute the closure of a set of elements
(defun lattice-closure (lattice set)
  (let ((closure set))
    (loop
      for element being the hash-keys of (elements-of lattice)
      unless (member element closure)
      when (every (lambda (x) (lattice-member-p lattice x)) (cons element closure))
      do (push element closure))
    closure))