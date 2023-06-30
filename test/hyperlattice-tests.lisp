(in-package :cl-user)

(defpackage hyperlattices/test
  (:use cl asdf parachute)
  (:export #:hyperlattices-test-suite))

(in-package :hyperlattices/test)

(define-test hyperlattices-test-suite)

(define-test (hyperlattices-test-suite sanity-check)
  (is = 1 1)
  (isnt = 1 2))

(test 'hyperlattices-test-suite)