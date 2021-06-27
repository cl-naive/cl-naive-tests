(defsystem "cl-naive-tests.tests"
  :description "Tests for cl-naive-tests."
  :version "2021.6.26"
  :author "Phil Marneweck <phil@psychedelic.co.za>"
  :licence "MIT"
  :depends-on (:cl-naive-tests)
  :components (
	       (:file "tests/package")
	       (:file "tests/tests" :depends-on ("tests/package"))))

