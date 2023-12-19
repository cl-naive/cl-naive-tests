(in-package :common-lisp-user)

(defpackage :cl-naive-tests
  (:use :cl)
  (:export
   :*debug*
   :*verbose*
   :*use-color*
   :*junit-no-properties*
   :*test-suites*
   :*suites-results*
   :disable-testcase
   :skip-testcase
   :skip-testcase-reason
   :testsuite
   :testcase
   :testsuite-names
   :testsuite-selection
   :create-suite
   :setup-suite
   :tear-down-suite
   :define-suite
   :run
   :report
   :statistics
   :find-testcase
   :calc-stats
   :format-results
   :write-results
   :save-results))
