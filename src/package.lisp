(in-package :common-lisp-user)

(defpackage :cl-naive-tests
  (:use :cl)
  (:export
   :*debug*
   :*verbose*
   :*junit-no-properties*
   :*test-suites*
   :*suites-results*
   :disable-testcase
   :skip-testcase
   :skip-testcase-reason
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
