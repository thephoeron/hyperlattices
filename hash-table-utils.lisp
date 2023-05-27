(in-package :cl-user)

(defpackage hyperlattice/hash-table-utils
  (:use cl alexandria)
  (:export #:puthash
           #:merge-hash-tables
           #:intersection-hash-tables
           #:hash-keys))

(in-package :hyperlattice/hash-table-utils)

(defun hash-keys (table)
  (let ((keys nil))
    (maphash (lambda (key value)
               (declare (ignore value))
               (push key keys))
             table)
    keys))

(defun puthash (key value table)
  (setf (gethash key table) value))

(defun merge-hash-tables (table1 table2)
  (let ((result (copy-hash-table table1)))
    (maphash (lambda (key value)
               (puthash key value result))
             table2)
    result))

(defun intersection-hash-tables (table1 table2)
  (let ((result (make-hash-table :test 'equal)))
    (maphash (lambda (key value)
               (when (gethash key table2)
                 (puthash key value result)))
             table1)
    result))