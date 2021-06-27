(in-package :common-lisp-user)

(defpackage :cl-naive-tests
  (:use :cl)
  (:export

   :find-result
   :calc-stats
   :*tests*
   :test
   :register-test
   :run
   :report
   :statistics
   :format-results
   :write-results))
