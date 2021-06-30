(in-package :cl-naive-tests)

(defparameter *test-suites* (make-hash-table :test #'equal)
  "Tests are stored in here when registered.")

(defparameter *suite-results* nil
  "The results of the last testsuite ran.")

(defmacro testsuite (identifier &body body)
  "Defines a TESTSUITE.
The IDENTIFIER is a symbol identifying the testsuite.
The BODY is a list of lisp forms or TESTCASE forms.
The results of the TESTCASE are collected as results of the TESTSUITE.
"
  `(setf (gethash ',identifier *test-suites*)
         (lambda (testsuite)
           (let ((*results* '()))
             (handler-case
                 (block ,identifier
                   ,@body)
               (error (err)
                 (push (list :identifier ',identifier
	                         :info       "Testsuite failure."
                             :expression ',identifier
                             :actual     err
	                         :result     nil)
                       *results*)))
             (nreverse *results*)))))

(defmacro testcase (identifier &key test-func test-data (equal 'equal) expected actual info)
  "The TEST macro evaluates the ACTUAL expression and compare its result with the EXPECTED expression.

The comparison is done either with the TEST-FUNC if provided, with the
EQUAL function.  If the comparison returns true, the test is
successful, otherwise it's a failure.  Test runs the test func
returning a plist with information about the result and test.

A plist containing the test info and results is returned.  It should
be used in the lambda that is registered with register-test.

IDENTIFIER is must be a symbol or a list of symbols.  By making it a
list of symbols the user is telling the system that the test is part
of a hierarchy of tests.  Statistics are calculated for the hierachy.
A example of a hierarchy would be like junit wants: suites, suite and
testcase.

TEST-FUNC is the function that is run to determine the result of the
test.  If none is supplied, then the EQUAL function is used to compare
the EXPECTED and the ACTUAL values.  The TEST-FUNC returns a
failure-type: T or :SUCCESS in case of success, NIL or :FAILURE in
case of failure, or some other keyword if the test wasn't run
successfully, but this shouldn't be counted as a failure.  The
TEST-FUNC is given two arguments: a plist containing :TEST-DATA
:EXPECTED :ACTUAL :EXPRESSION :ERROR, and the INFO. (:ACTUAL is the
value of the :EXPRESSION that is tested; if an error is signaled, it's
passed in :ERROR).

TEST-DATA is a convenient place to store data the test relies on, this
can be used during the test and later in reporting on test results.
You can put what ever you want to in it.

INFO is a string to be read by the human that is digging into the
tests results, describing the test.

EXAMPLE:

    (test (division non-zero-dividend)
          :equal '=
          :expected 3/2
          :actual (/ 3 2)
          :info \"Integer division by non-zero, giving a ratio.\")

    (test (division by-zero)
          :test-func (lambda (result info)
                       (let ((actual (getf result :actual)))
                         (cond ((eql (getf result :expected)
                                :success)
                               ((typep actual 'error)
                                (setf (aref (getf result :test-data) 0)
                                      (list 'unexpected-error (type-of actual)))
                                :failure)
                               (t
                                (setf (aref (getf result :test-data) 0)
                                      (list 'unexpected-result actual))
                                :failure))))
          :test-data (vector nil)
          :expected 'division-by-zero
          :actual (handler-case (/ 3 0)
                    (:no-error (result) result)
                    (division-by-zero () 'division-by-zero)
                    (error (err) err))
          :info \"Integer division by zero, giving a DIVISION-BY-ZERO error.\")
"
  (let ((videntifier  (gensym))
        (vtest-func   (gensym))
        (vtest-data   (gensym))
        (vequal       (gensym))
        (vexpected    (gensym))
        (vexpression  (gensym))
        (vresult      (gensym))
        (vinfo        (gensym))
        (vtest-result (gensym))
        (verror       (gensym))
        (vsysout      (gensym))
        (vsyserr      (gensym)))
    `(let ((,videntifier ',identifier)
           (,vtest-func  ,test-func)
           (,vequal      ,equal)
           (,vinfo       ,info)
           (,vexpected   ,expected)
           (,vexpression ',actual)
           (,verror      nil)
           (,vsysout     nil)
           (,vsyserr     nil)
           (,vresult     nil))
       (let* ((,vresult      (handler-case
                                 (progn
                                   (setf ,vsysout
                                        (with-output-to-string (*standard-output*)
                                          (setf ,vsyserr
                                                (with-output-to-string (*error-output*)
                                                  (let ((*trace-output* *error-output*))
                                                    (setf ,vresult (progn ,actual)))))))
                                   ,vresult)
                               (:no-error (result)
                                 result)
                               (error (err)
                                 (setf ,verror t)
                                 err)))
              (*testcase* (list :identifier   ,videntifier
	                            :info         ,vinfo
                                :equal        ,vequal
                                :test-func    ,vtest-func
                                :test-data    ,vtest-data
	                            :expected     ,vexpected
                                :expression   ,vexpression
                                :actual       ,vresult
                                :error        (if ,verror ,vresult nil)
                                :sysout       ,vsysout
                                :syserr       ,vsyserr))
              (,vtest-result (let ((,vtest-result
                                     (cond
                                       (,vtest-func (funcall ,vtest-func *testcase* ,vinfo))
                                       (,verror     nil)
                                       (,vequal     (not (not (funcall ,vequal ,vexpected ,vresult))))
			                           (t           (not (not (equal ,vexpected ,vresult)))))))
                               (cond
                                 (,verror               :error)
                                 ((null ,vtest-result)  :failure)
                                 ((eql ,vtest-result t) :success)
                                 (t                     ,vtest-result)))))
	     (setf (getf *testcase* :result)       ,vtest-result
               (getf *testcase* :failure-type) ,vtest-result)
         (push *testcase* *results*)
         ,vtest-result))))

(defun print-testcases (testcases &key (verbose t)
                                    ((:output *standard-output*) *standard-output*))
  "When VERBOSE is NIL don't report the successes."
  (dolist (testcase testcases)
    (case (getf testcase :failure-type)
      ((:success)
       (when verbose
         (format t "~%Test ~A:~40T SUCCESS~%" (getf testcase :identifier))))
      ((:failure)
       (format t "~%Test ~A:~40T FAILURE~%" (getf testcase :identifier))
       (format t "~&Failure:       The expression: ~S~@
                 ~&                  evaluates to: ~S~@
                 ~&                  which is not  ~A~@
                 ~&      to the expected testcase: ~S~%"
               (getf testcase :expression)
               (getf testcase :actual)
               (or (getf testcase :equal) (getf testcase :test-func) 'equalp)
               (getf testcase :expected)))
      ((:error)
       (format t "~%Test ~A:~40T ERROR~%"   (getf testcase :identifier))
       (format t "~&Error:         The expression: ~S~@
                 ~&             signaled an error: ~A~@
                 ~&     the expected testcase was: ~S~%"
               (getf testcase :expression)
               (getf testcase :error)
               (getf testcase :expected)))
      (otherwise
       (format t "~%Test ~A:~40T ~A~%"
               (getf testcase :identifier)
               (getf testcase :failure-type))))
    (when verbose
	  (format t "Data:~32T~S~%Info:~32T~S~%"
			  (getf testcase :test-data)
			  (getf testcase :info)))
    (unless (eql :success (getf testcase :failure-type))
      (format t "~%"))))

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
		            (list identifier)))
      (let* ((stat-id (if parent
			              (append parent id)
			              id))
	         (stat (gethash stat-id stats)))
        (incf (getf stat :count 0))
	    (if (getf result :result)
            (incf (getf stat :passed 0))
            (incf (getf stat (get result :failure-type 0))))
	    (setf (gethash stat-id stats) stat)
	    (setf parent (list id))))))

(defun run (&key (suites *test-suites*) keep-stats-p)
  "Runs all the testcases in all the SUITES passed in or all testsuites registered.
Statistics can be calculated during a test run, but the default is to use statistics after a test run to calculate stats."
  (let ((suite-results '())
	    (stats         (and keep-stats-p (make-hash-table :test 'equalp))))
    (maphash (lambda (key value)
	           (let ((results (funcall value key)))
		         (when keep-stats-p
                   (dolist (result results)
		            (calc-stats result stats)))
		         (setf suite-results (nconc suite-results results))))
	         suites)
    (setf *suite-results* suite-results)
    (values suite-results stats)))

(defun report (&optional (results *suite-results*))
  "Reports on the pass or failure of the results set over all. This does not do any pretty printing etc because it needs to be machine readable. If you want pretty reporting look at format-results or do your own."
  (let ((passed)
	    (failed)
	    (other))
    (dolist (result results)
      (case (getf result :failure-type)
        ((:success) (push result passed))
        ((:failure) (push result failed))
        (otherwise  (push result (getf other (getf result :failure-type) '())))))
    (format t "~&Passed:~12T~4D~%Failed:~12T~4D~%" (length passed) (length failed))
    (loop :for (key others) :on other :by (function cddr)
          :do (format t "~A:~12T~4D~%" key (length others)))
    (print-testcases results :verbose nil) ; only report errors and failures.
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

(defun junit-format-testcase (testcases)
  (dolist (testcase testcases)
	(cl-who:htm
	 (:testcase
      :id (getf testcase :identifier)
	  :name (getf testcase :identifier)
      (case (getf testcase :failure-type)
        ((:success))
        ((:failure)
         ;; <failure message="" <!-- The message specified in the assert. -->
	     ;;          type=""    <!-- The type of the assert. -->
	     ;;      >failure description</failure>
         (cl-who:htm
		  (:failure
		   :message (getf testcase :info)
		   :type (prin1-to-string (or (getf testcase :test-func) (getf testcase :equal)))
		   (cl-who:str (with-output-to-string (out)
                         (print-testcases (list testcase) :output out))))))
        ((:error)
         ;; <error message="" <!-- The error message. e.g., if a java exception is thrown, the return value of getMessage() -->
         ;;        type=""    <!-- The type of error that occured. e.g., if a java execption is thrown the full class name of the exception. -->
         ;;      >error description</error>
         (cl-who:htm
		  (:error
		   :message (princ-to-string (getf testcase :error))
		   :type   (prin1-to-string (class-of (getf testcase :error)))
		   (cl-who:str (with-output-to-string (out)
                         (print-testcases (list testcase) :output out))))))
        (otherwise))))
    ;; <!-- Data that was written to standard out while the test was executed. optional -->
    ;; <system-out>STDOUT text</system-out>
    (cl-who:htm (:system-out (cl-who:str (getf testcase :sysout))))
    ;; <!-- Data that was written to standard error while the test was executed. optional -->
    ;; <system-err>STDERR text</system-err>
    (cl-who:htm (:system-err (cl-who:str (getf testcase :syserr))))))

(defun junit-format-testsuite (testsuite)
  (declare (ignore testsuite))          ; for now
  ;; <!-- Properties (e.g., environment settings) set during test execution.
  ;;      The properties element can appear 0 or once. -->
  ;; <properties>
  ;;   <!-- property can appear multiple times. The name and value attributres are required. -->
  ;;   <property name="" value=""/>
  ;; </properties>
  (cl-who:htm
   (:properties
    (:property :name "LISP-IMPLEMENTATION-TYPE"    :value (cl-who:str (lisp-implementation-type)))
    (:property :name "LISP-IMPLEMENTATION-VERSION" :value (cl-who:str (lisp-implementation-version)))
    (:property :name "SOFTWARE-TYPE"               :value (cl-who:str (software-type)))
    (:property :name "SOFTWARE-VERSION"            :value (cl-who:str (software-version)))
    (:property :name "MACHINE-INSTANCE"            :value (cl-who:str (machine-instance)))
    (:property :name "MACHINE-TYPE"                :value (cl-who:str (machine-type)))
    (:property :name "MACHINE-VERSION"             :value (cl-who:str (machine-version)))
    (:property :name "*FEATURES*"                  :value (cl-who:str *features*)))))

(defmethod format-results ((format (eql :junit)) results)
  "Formats then results as Junit XML, junits only allows 3 levels nl. suites, suite and testcase.
If your identifiers are not 1 or 3 levels this wont work for you."

  (cl-who:with-html-output-to-string (*standard-output* nil :indent t)
    "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
    (when results
      (let ((suite-p (not (member :result (first results)))))
	    (if suite-p

	        (dolist (suites results)
	          (cl-who:htm
	           (:testsuites
	            :id "auto-suites" :name "auto-suites"
	            (dolist (suite (getf suites :results))
		          (cl-who:htm
		           (:testsuite
		            :id (getf suite :identifier)
		            :name (getf suite :identifier)
                    (junit-format-testsuit suite)
                    (junit-format-testcase (getf suite :results))))))))

            (cl-who:htm
	         (:testsuites
	          :id "auto-suites" :name "auto-suites"
	          (:testsuite
	           :id "auto-suite" :name "suite"
               (junit-format-testsuite nil)
               (junit-format-testcase results)))))))))

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
