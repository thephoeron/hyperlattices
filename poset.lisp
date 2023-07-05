(in-package :cl-user)

(defpackage hyperlattices/partially-ordered-set
  (:nicknames hyperlattices/poset hl/poset hl-poset partially-ordered-set poset)
  (:use c2cl)
  (:export #:poset
           #:elements
           #:leq
           #:less-than-p
           #:less-than-or-equal-p
           #:greater-than-p
           #:greater-than-or-equal-p)
  (:documentation "Implementation of POSET class for partially-ordered sets."))

(in-package :hyperlattices/partially-ordered-set)

(defclass poset ()
  ((elements :accessor elements :initarg :elements :type list)
   (leq :accessor leq :initarg :leq :type function)))

(defmethod less-than-p ((lhs poset) (rhs poset) x y)
  (and (member x (elements lhs))
       (member y (elements lhs))
       (funcall (leq lhs) x y)))

(defmethod less-than-or-equal-p ((lhs poset) (rhs poset) x y)
  (or (equal x y)
      (less-than-p lhs rhs x y)))

(defmethod greater-than-p ((lhs poset) (rhs poset) x y)
  (less-than-p rhs lhs x y))

(defmethod greater-than-or-equal-p ((lhs poset) (rhs poset) x y)
  (less-than-or-equal-p rhs lhs x y))
