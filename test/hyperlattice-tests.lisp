(in-package :cl-user)

(defpackage hyperlattice/test
  (:use cl asdf parachute)
  (:export #:run-all-tests))

(in-package :hyperlattice/test)

(define-test hyperlattice-test-suite)

(define-test (hyperlattice-test-suite sanity-check)
  (is = 1 1)
  (isnt = 1 2))

(test 'hyperlattice-test-suite)