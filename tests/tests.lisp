(in-package :cl-naive-tests.tests)

;;;; If you're a purist you are going to have a heart attack if you
;;;; look at this.  I am using the package I want to test to test
;;;; itself :P

(setf *tests* (make-hash-table :test #'equalp))

(register-test :test-pass
	       (lambda (key)
		 (test key
		       :data (list :expected t
				   :actual t)
		       :info "(equalp t t)")))

(register-test :test-pass-2
	       (lambda (key)
		 (test key
		       :test-func #'(lambda (data info)
				      (declare (ignore data info))
				      (equalp t t))
		       :info "(equalp t t)")))

(register-test :test-fail
	       (lambda (key)
		 (test key
		       :failure-type :ignore
		       :data (list :expected nil
				   :actual t)
		       :info "(equalp nil t)")))

(register-test :test-fail-2
	       (lambda (key)
		 (test key
		       :failure-type :ignore
		       :test-func #'(lambda (data info)
				      (declare (ignore data info))
				      (equalp nil t))
		       :info "(equalp nil t)")))

(register-test :test-result
	       (lambda (key)
		 (test key
		       :data (list :expected '(:IDENTIFIER :EISH
					       :FAILURE-TYPE :FAILURE
					       :RESULT T
					       :DATA (:EXPECTED T :ACTUAL T)
					       :INFO "(equalp t t)")
				   :actual (test :eish
						 :data (list :expected t
							     :actual t)
						 :info "(equalp t t)"))
		       :info "(equalp nil t)")))

(register-test :test-result-format
	       (lambda (key)
`		 (test key
		       :data (list :expected "((:IDENTIFIER :EISH :FAILURE-TYPE :FAILURE :RESULT T :DATA
  (:EXPECTED T :ACTUAL T) :INFO \"(equalp t t)\"))"
				   :actual (format-results nil
							   (list (test :eish
								       :data (list :expected t
										   :actual t)
								       :info "(equalp t t)"))))
		       :info "(equalp nil t)")))

(register-test :test-result-format-junit
	       (lambda (key)
		 (test key
		       :data (list :expected "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>
<testsuites id='auto-suites' name='auto-suites'>
  <testsuite id='auto-suite' name='suite'>
<testcase id='EISH' name='EISH'>
</testcase>
  </testsuite>
</testsuites>"
				   :actual (format-results :junit
							   (list (test :eish
								       :data (list :expected t
										   :actual t)
								       :info "(equalp t t)"))))
		       :info "(equalp nil t)")))

(register-test :test-result-format-junit-fail
	       (lambda (key)
		 (test key
		       :data (list :expected "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>
<testsuites id='auto-suites' name='auto-suites'>
  <testsuite id='auto-suite' name='suite'>
<testcase id='EISH' name='EISH'>
<failure message='EISH' type='FAILURE'>Data:
(:EXPECTED T :ACTUAL NIL)

Info:
\"(equalp t nil)\"

</failure>
</testcase>
<testcase id='HSIE' name='HSIE'>
</testcase>
  </testsuite>
</testsuites>"
				   :actual (format-results :junit
							   (list (test :eish
								       :data (list :expected t
										   :actual nil)
								       :info "(equalp t nil)")
								 (test :hsie
								       :data (list :expected t
										   :actual t)
								       :info "(equalp t t)"))))
		       :info "(equalp nil t)")))

;;(report (run))
