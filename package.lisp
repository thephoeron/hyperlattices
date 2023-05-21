(in-package :cl-user)

(defpackage hyperlattice
  (:use cl alexandria)
  (:export #:puthash
           #:merge-hash-tables
           #:intersection-hash-tables))