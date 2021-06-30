(in-package :common-lisp-user)

(defpackage :cl-naive-tests
  (:use :cl)
  (:export
   :testsuite
   :testcase
   :run
   :report
   :statistics
   :find-result
   :calc-stats
   :format-results
   :write-results))
