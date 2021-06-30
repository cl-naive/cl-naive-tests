(in-package :common-lisp-user)

(defpackage :cl-naive-tests
  (:use :cl)
  (:export
   :*debug*
   :*verbose*
   :*suites-results*
   :testsuite
   :testcase
   :run
   :report
   :statistics
   :find-testcase
   :calc-stats
   :format-results
   :write-results
   :save-results
   ))
