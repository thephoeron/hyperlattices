(in-package :hyperlattice)

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