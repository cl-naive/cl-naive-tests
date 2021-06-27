(defsystem "cl-naive-tests"
  :description "A test framework that is not based on any of the mainstream popular testing frameworks. It has a very simple api, so the learning curve should be considerably less than for most other frameworks. It is also designed to work well with gitlab CI."
  :version "2021.6.26"
  :author "Phil Marneweck <phil@psychedelic.co.za>"
  :licence "MIT"
  :depends-on (:cl-who)
  :components (
	       (:file "src/package")
	       (:file "src/naive-tests" :depends-on ("src/package"))))

