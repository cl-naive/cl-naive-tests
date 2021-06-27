(in-package :cl-naive-tests)

(defun find-result (test-identifier results &key test)
  "Finds a result in the results."
  (and results
       (find test-identifier
	     (getf results :results)
	     :test (or test #'equalp)
	     :key (lambda (result)
		    (when (listp result)
		      (getf result :identifier))))))

(defun calc-stats (result &optional (stats (make-hash-table :test #'equalp)))
  "Calculates stats. Stats are simple counts of tests, passed and failed per level.
Stats are stored in a hashtable per identifier level, which makes it easy to get to in format-results if needed."
  (let ((identifier (getf result :identifier))
	(parent nil))
    (dolist (id (if (listp identifier)
		    identifier
		    (listp identifier)))
      (let* ((stat-id (if parent
			  (append parent id)
			  id))
	     (stat (gethash stat-id stats)))
	(setf (getf stat :count)
	      (if (getf stat :count)
		  (incf (getf stat :count))
		  1))
	(if (getf result :result)
	    (setf (getf stat :passed)
		  (if (getf stat :passed)
		      (incf (getf stat :passed))
		      1))
	    (setf (getf stat (get result :failure-type))
		  (if (getf stat (get result :failure-type))
		      (incf (getf stat (get result :failure-type)))
		      1)))
	(setf (gethash stat-id stats) stat)
	(setf parent (list id))))))

(defun test (identifier &key test-func data info (failure-type :failure))
  "Test runs the test func returning a plist with information about the result and test.
It should be used in the lambda that is registered with register-test.

identifier is must be a keyword or a list of keywords. By making it a list of keywords the user is telling the system that the test is part of a hierarchy of tests. Stats are calculated for the hierachy. A example of a hierarchy would be like junit wants nl suites, suite and testcase.

test-func is the function that is run to determine the result of the test. If none is supplied and data is supplied then it defaults to (equalp (getf data :expected) (getf data :actual)).

data is a convenient place to store data the test relies on, this can be used during the test and later in reporting on test results. Unless you want to use the default behaviour of test-func you can put what ever you want to in it. If you do want to use the default behaviour of test-func data should be a plist with keys of :expected and :actual.

info is there for the human that is digging into the tests results, you can put what ever you think will be useful to such a person in there.

failure-type gives the user the opportunity to have tests that can fail but wont fail the overall test suite. Think of it as warnings and stuff like that. naive-tests uses :failure as the default type and its the only type that will cause a test suite to fail when you run report.
"
  (list :identifier identifier
	:failure-type (or failure-type :failed)
	:result (if test-func
		    (funcall test-func data info)
		    (when data
		      (equalp (getf data :expected)
			      (getf data :actual))))
	:data data
	:info info))

(defparameter *tests* (make-hash-table :test #'equalp)
  "Tests are stored in here when registered.")

(defun register-test (identifier test-lambda)
  "Saves a test so it can be run later in a batch."
  (setf (gethash identifier *tests*) test-lambda))

(defun run (&key (tests *tests*) keep-stats-p)
  "Runs all tests passed in or all tests registered.
Statistics can be calculated during a test run, but the default is to use statistics after a test run to calculate stats."
  (let ((results)
	(stats (and keep-stats-p (make-hash-table :test 'equalp))))
    (maphash (lambda (key value)
	       (let ((result (funcall value key)))
		 (when keep-stats-p
		   (calc-stats result stats))
		 (push result results)))
	     tests)
    (values results stats)))

(defun report (results)
  "Reports on the pass or failure of the results set over all. This does not do any pretty printing etc because it needs to be machine readable. If you want pretty reporting look at format-results or do your own."
  (let ((passed)
	(failed)
	(other)
	(other-count 0))

    (dolist (result results)
      (if (getf result :result)
	  (push result passed)
	  (if (equalp (getf result :failure-type) :failed)
	      (push result failed)
	      (progn
		(incf other-count)
		(if (getf other (getf result :failure-type))
		    (push result (getf other (getf result :failure-type)))
		    (setf  (getf other (getf result :failure-type)) (list result)))))))

    (format t "Passed:~A~%Failed:~A~%Other:~A~%" (length passed) (length failed) other-count)
    (values (not failed) passed failed other)))

(defun statistics (results)
  "Can be used to calculate statistics post tests if *keep-stats-p* was nil."
  (let ((stats (make-hash-table :test #'equalp)))
    (dolist (result results)
      (calc-stats result stats))
    stats))

(defgeneric format-results (format results)
  (:documentation "Formats the results according to format.
The default method just outputs the results using lisp format string."))

(defmethod format-results (format results)
  (format nil "~S" results))

(defmethod format-results ((format (eql :junit)) results)
  "Formats then results as Junit XML, junits only allows 3 levels nl. suites, suite and testcase.
If your identifiers are not 1 or 3 levels this wont work for you."

  (cl-who:with-html-output-to-string (*standard-output* nil :indent t)
    "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
    (when results
      (let ((suite-p (not (member :result (first results)))))

	(unless suite-p
	  (cl-who:htm
	   (:testsuites
	    :id "auto-suites" :name "auto-suites"
	    (:testsuite
	     :id "auto-suite" :name "suite"
	     (dolist (result results)
	       (cl-who:htm
		(:testcase
		 :id (getf result :identifier)
		 :name (getf result :identifier)
		 (unless (getf result :result)

		   (cl-who:htm
		    (:failure
		     :message (getf result :identifier)
		     :type (getf result :failure-type)
		     (cl-who:str
		      (format
		       nil
		       "Data:~%~S~%~%Info:~%~S~%"
		       (getf result :data)
		       (getf result :info)))))))))))))

	(when suite-p
	  (dolist (suites results)
	    (cl-who:htm
	     (:testsuites
	      :id "auto-suites"
	      :name "auto-suites"

	      (dolist (suite (getf suites :results))
		(cl-who:htm
		 (:testsuites
		  :id (getf suite :identifier)
		  :name (getf suite :identifier)

		  (dolist (testcase (getf suite :results))
		    (cl-who:htm
		     (:testcase :id (getf testcase :identifier)
				:name (getf testcase :identifier)
				(unless (getf testcase :result)
				  (cl-who:htm
				   (:failure
				    :message (getf testcase :identifier)
				    :type (getf testcase :failure-type)
				    (cl-who:str
				     (format
				      nil
				      "Data:~%~S~%~%Info:~%~S~%"
				      (getf testcase :data)
				      (getf testcase :info))))))))))))))))))))

(defun write-results (results &key (file "results.log") format)
  "Writes results to file. If format is supplied formats results first using format-results.
This is used to produce files that could be used by some thing like gitlab CI."
  (with-open-file (stream file
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (print (if format
	       (format-results format results)
	       results)
	   stream)))
