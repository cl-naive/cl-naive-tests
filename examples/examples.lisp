(in-package :cl-user)

(defpackage :cl-naive-tests-examples
  (:use :cl :cl-naive-tests))

(in-package :cl-naive-tests-examples)

;; Define a test suite
(testsuite math-tests
  ;; Test case 1: Addition
  (testcase addition-test
            :actual (+ 2 2)
            :expected 4
            :info "Test basic addition.")

  ;; Test case 2: Subtraction
  (testcase subtraction-test
            :actual (- 5 3)
            :expected 2
            :info "Test basic subtraction.")

  ;; Test case 3: Multiplication (intentional failure)
  (testcase multiplication-test
            :actual (* 3 4)
            :expected 10
            :info "Test basic multiplication (should fail).")

  ;; Test case 4: Division by zero (error handling)
  (testcase division-by-zero-test
            :test-func (lambda (result info)
                         (declare (ignore info))
                         (typep (getf result :actual) 'division-by-zero))
            :actual (handler-case (/ 1 0)
                      (division-by-zero (err) err))
            :expected t
            :info "Test division by zero error handling."))

;; To run the tests and see the report:
;; (ql:quickload :cl-naive-tests-examples)
;; (cl-naive-tests:report (cl-naive-tests:run :suites '(math-tests)))

