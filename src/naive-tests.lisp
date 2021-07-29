(in-package :cl-naive-tests)

(defparameter *test-suites* (make-hash-table :test #'equal)
  "Tests are stored in here when registered.")

(defvar *suites-results* nil
  "The result of the last testsuites run.")

(defvar *suite-results* nil
  "The results of the last testsuite ran.")

(defvar *testsuite-name* nil
  "The current testsuite name.")

(defvar *testcase-results* nil
  "The current testcase results (plist).")

(defvar *debug* nil
  "Set to true to break into the debugger on errors.")

(defvar *verbose* nil
  "When true, the success testcases are also printed.")

(defvar *junit-no-properties* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun readable-string (string)
    (coerce string '(simple-array character (*)))))

(defun iso8601-time-stamp (&optional (time (get-universal-time)))
  (multiple-value-bind (se mi ho da mo ye) (decode-universal-time time 0)
    (readable-string
     (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
             ye mo da ho mi se))))

(defvar *test-count*)
(defvar *disabled-count*)
(defvar *error-count*)
(defvar *failure-count*)
(defvar *skipped-count*)
(defvar *test-package*)

(defmacro testsuite (identifier &body body)
  "Defines a TESTSUITE.
The IDENTIFIER is a symbol identifying the testsuite.
The BODY is a list of lisp forms or TESTCASE forms.
The results of the TESTCASE are collected as results of the TESTSUITE.
"
  (let ((fname (gensym (string identifier))))
    `(progn
       (defun ,fname (*testsuite-name*)
         (let ((*suite-results*      '())
               (*test-count*   0)
               (*disabled-count* 0)
               (*error-count*    0)
               (*failure-count*  0)
               (*skipped-count*  0)
               (*test-package*   nil)
               (start-time     (get-universal-time)))
           (if *debug*
               (handler-bind ((error (function invoke-debugger)))
                 (block ,identifier
                   ,@body))
               (handler-case
                   (block ,identifier
                     ,@body)
                 (error (err)
                   (incf *error-count*)
                   (push (list :identifier ',identifier
	                       :info       "Testsuite failure."
                               :expression ',identifier
                               :actual     err
	                       :result     nil
                               :error      err
                               :failure-type :error)
                         *suite-results*))))
           (list :identifier ',identifier
                 :tests     *test-count*
                 :errors    *error-count*
                 :failures  *failure-count*
                 :disabled  *disabled-count*
                 :skipped   *skipped-count*
                 :hostname  (machine-instance)
                 :package   (or *test-package* (readable-string (package-name *package*)))
                 :time      (- (get-universal-time) start-time)
                 :timestamp (iso8601-time-stamp start-time)
                 :testcases (nreverse *suite-results*))))
       (eval-when (:load-toplevel :execute)
         (setf (gethash ',identifier *test-suites*) (function ,fname)))
       ',identifier)))


(define-condition disable-testcase (condition)
  ()
  (:report "Disable the current testcase"))
(declaim (inline disable-testcase))
(defun disable-testcase () (signal 'disable-testcase))

(define-condition skip-testcase (condition)
  ((reason :initarg :reason :reader skip-testcase-reason))
  (:report (lambda (condition stream)
             (format stream "Skip the current testcase for ~A"
                     (skip-testcase-reason condition)))))
(declaim (inline skip-testcase))
(defun skip-testcase (&optional (message "Test case is skipped."))
  (signal 'skip-testcase :reason message))

(defun prepare-filename (pathname)
  (let ((ci-project-dir (uiop:getenv "CI_PROJECT_DIR"))
        (asdf-output-translations (namestring (asdf:apply-output-translations "/")))
        (namestring (namestring pathname)))
    (let ((namestring
            (namestring (make-pathname :type "lisp"
                                       :defaults (if (string= namestring asdf-output-translations :end1 (length asdf-output-translations))
                                                     ;; in asdf-output-translations:
                                                     (subseq namestring (1- (length asdf-output-translations)))
                                                     ;; outside:
                                                     namestring)))))
      (if ci-project-dir
          (enough-namestring namestring (format nil "~A/" ci-project-dir))
          namestring))))

(defun %testcase (identifier disabled test-func test-data equal expected actual expression info package)
  (let (error
        sysout
        syserr
        result)
    (setf *test-package* (readable-string (package-name package)))
    (incf *test-count*)
    (let* ((result (if disabled
                       (setf error :disabled)
                       (handler-case
                           (progn
                             (setf sysout
                                   (with-output-to-string (*standard-output*)
                                     (setf syserr
                                           (with-output-to-string (*error-output*)
                                             (let ((*trace-output* *error-output*))
                                               (let ((filename  (prepare-filename (load-time-value *load-truename*))))
                                                 (format t "[[ATTACHMENT|~A]]~%" filename))
                                               (setf result (funcall actual)))))))
                             result)
                         (:no-error (result)
                           result)
                         (disable-testcase ()
                           (setf error :disabled))
                         (skip-testcase (condition)
                           (setf result (skip-testcase-reason condition)
                                 error :skipped))
                         (error (err)
                           (setf error t)
                           err))))
           (*testcase-results* (list :identifier   identifier
	                             :info         info
                                     :equal        equal
                                     :test-func    test-func
                                     :test-data    test-data
	                             :expected     expected
                                     :expression   expression
                                     :actual       result
                                     :failure-type (case error
                                                     ((:disabled) :disabled)
                                                     ((:skipped)  :skipped)
                                                     ((t)         :error)
                                                     (otherwise   nil))
                                     :error        (if (eql t error)
                                                       result
                                                       nil)
                                     :sysout       sysout
                                     :syserr       syserr))
           (test-result (or (getf *testcase-results* :failure-type)
                            (let ((test-result
                                    (handler-case
                                        (cond
                                          (test-func (funcall test-func *testcase-results* info))
                                          (error     nil)
                                          (equal     (not (not (funcall equal expected result))))
			                  (t           (not (not (equal expected result)))))
                                      (disable-testcase ()
                                        (setf error :disabled)
                                        :disabled)
                                      (skip-testcase (condition)
                                        (setf result (skip-testcase-reason condition)
                                              error :skipped)
                                        :skipped)
                                      (error (err)
                                        (setf result err
                                              error :error)))))
                              (cond
                                (error               :error)
                                ((null test-result)  :failure)
                                ((eql test-result t) :success)
                                (t                     test-result))))))
      (case test-result
        ((:error)    (incf *error-count*)   (setf (getf *testcase-results* :error)  result))
        ((:failure)  (incf *failure-count*))
        ((:skipped)  (incf *skipped-count*) (setf (getf *testcase-results* :reason) result))
        ((:disabled) (incf *disabled-count*)))
      (setf (getf *testcase-results* :result)       test-result
            (getf *testcase-results* :failure-type) test-result)
      (push *testcase-results* *suite-results*)
      test-result)))

(defmacro testcase (identifier &key disabled test-func test-data (equal ''equal) expected actual info)
  ;; Better to use a function name than a function for equal, because it's used in reports.
  "The TEST macro evaluates the ACTUAL expression and compare its result with the EXPECTED expression.

The comparison is done either with the TEST-FUNC if provided, with the
EQUAL function.  If the comparison returns true, the test is
successful, otherwise it's a failure.  Test runs the test func
returning a plist with information about the result and test.

A plist containing the test info and results is returned.  It should
be used in the lambda that is registered with register-test.

IDENTIFIER must be a symbol naming the testcase.

DISABLED is a generalized-boolean expression, evaluated. If the result
is true, then the test is marked disabled, and not run.

The ACTUAL code, or the TEST-FUNC code can also disable the testcase
by signaling a DISABLE-TESTCASE condition, or skip the testcase with a
message by signaling a SKIP-TESTCASE condition. (Functions are
provided to easily signal those conditions).

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
  `(%testcase ',identifier ',disabled
              ,test-func
              ,test-data ,equal ,expected (lambda () ,actual) ',actual ,info ,*package*))



;;; ========================================
;;; formatting results
;;; ========================================

(defgeneric format-results (format results)
  (:documentation "Formats the results according to format.
The default method just outputs the results using lisp format string."))

(defmethod format-results (format suites-results)
  (declare (ignorable format))
  (format nil "~S" suites-results))

(defun write-results (suites-results &key (stream *standard-output*) format)
  "Writes results to STREAM. Formats the results using FORMAT-RESULT."
  (write-string (format-results format suites-results) stream))

(defun save-results (suites-results &key (file "results.log") format)
  "Writes results to file. Formats the results using FORMAT-RESULT."
  (ensure-directories-exist file)
  (with-open-file (stream file
			              :direction :output
			              :if-exists :supersede
			              :if-does-not-exist :create)
    (write-results suites-results :stream stream :format format)))


;;; ========================================
;;; Running and Reporting testsuites
;;; ========================================

(defun find-testcase (test-identifier suites &key test)
  "Finds a result in the suites."
  (dolist (suite (getf suites :suites))
    (let ((testcase (find test-identifier
	                      (getf suite :testcases)
	                      :test (or test #'equalp)
	                      :key (lambda (testcase)
		                         (when (listp testcase)
		                           (getf testcase :identifier))))))
      (when testcase (return-from find-testcase testcase)))))

(defun calc-stats (suites &optional (stats (make-hash-table :test #'equalp)))
  "Calculates stats. Stats are simple counts of tests, passed and failed per level.
Stats are stored in a hashtable per identifier level, which makes it easy to get to in format-results if needed."
  (let ((suites-path (list (getf suites :name))))
    (dolist (suite (getf suites :suites))
      (let ((suite-path (append suites-path (list (getf suite :identifier)))))
        (dolist (testcase (getf suite :testcases))
          (let ((testcase-path (append suite-path (list (getf testcase :identifier)))))
            (dolist (path (list suites-path suite-path testcase-path))
              (let ((stat (gethash path stats)))
                (incf (getf stat :count 0))
                (incf (getf stat (getf testcase :failure-type) 0))
                (setf (gethash path stats) stat)))))))))

(defun run (&key (suites *test-suites*) keep-stats-p ((:debug *debug*) nil) (name ':suites))
  "Runs all the testcases in all the SUITES passed in or all testsuites registered.
Statistics can be calculated during a test run, but the default is to use statistics after a test run to calculate stats."
  (let ((suite-results '())
	    (stats         (and keep-stats-p (make-hash-table :test 'equalp))))
    ;; Run the testsuites:
    (maphash (lambda (key value) (push (funcall value key) suite-results)) suites)
    ;; Build the suites-results:
    (let ((suites (list :name     name
                        :disabled (reduce (function +) suite-results :key (lambda (suite) (getf suite :disabled 0)))
                        :skipped  (reduce (function +) suite-results :key (lambda (suite) (getf suite :skipped  0)))
                        :errors   (reduce (function +) suite-results :key (lambda (suite) (getf suite :errors   0)))
                        :failures (reduce (function +) suite-results :key (lambda (suite) (getf suite :failures 0)))
                        :tests    (reduce (function +) suite-results :key (lambda (suite) (getf suite :tests    0)))
                        :time     (reduce (function +) suite-results :key (lambda (suite) (getf suite :time     0)))
                        :suites   (nreverse suite-results))))
      (when keep-stats-p
        ;; Compute the statitics:
        (calc-stats suites stats))
      (setf *suites-results* suites)
      (values suites stats))))

(defun report (&optional (suites-results *suites-results*))
  "Reports on the pass or failure of the results set over all. This does not do any pretty printing etc because it needs to be machine readable. If you want pretty reporting look at format-results or do your own."
  (let ((passed '())
	    (failed '())
	    (other  '()))
    (dolist (suite (getf suites-results :suites))
      (dolist (testcase (getf suite :testcases))
        (case (getf testcase :failure-type)
          ((:success) (push testcase passed))
          ((:failure) (push testcase failed))
          (otherwise  (push testcase (getf other (getf testcase :failure-type) '()))))))
    (format t "~&Passed:~12T~4D~%Failed:~12T~4D~%" (length passed) (length failed))
    (loop :for (key others) :on other :by (function cddr)
          :do (format t "~A:~12T~4D~%" key (length others)))
    (values (not failed) passed failed other)))

(defun statistics (suites-results)
  "Can be used to calculate statistics post tests if *keep-stats-p* was nil."
  (let ((stats (make-hash-table :test #'equalp)))
    (calc-stats suites-results stats)
    stats))

;;; ========================================
;;; TEXT format
;;; ========================================

(defun split-lines (text)
  (loop
    :for start := 0 :then (and end (1+ end))
    :for end := (when start (position #\newline text :start start))
    :while start
    :collect (subseq text start end)))

(defun print-testcase-result (testcase &key (verbose *verbose*)
                                         ((:output *standard-output*) *standard-output*))
  (case (getf testcase :failure-type)
    ((:success)
     (when verbose
       (format t "Testcase ~A:~40T SUCCESS~%" (getf testcase :identifier))))
    ((:failure)
     (format t "Testcase ~A:~40T FAILURE~%" (getf testcase :identifier))
     (format t "Failure:        The expression: ~S~@
              ~&                  evaluates to: ~S~@
              ~&                  which is not  ~A~@
              ~&        to the expected result: ~S~%"
             (getf testcase :expression)
             (getf testcase :actual)
             (let ((test-func (getf testcase :test-func)))
               (if test-func
                   (if (symbolp test-func)
                       test-func
                       "accepted by the test-func")
                   (or (getf testcase :equal) 'equalp)))
             (getf testcase :expected)))
    ((:error)
     (format t "Testcase ~A:~40T ERROR~%"   (getf testcase :identifier))
     (format t "Error:          The expression: ~S~@
              ~&             signaled an error: ~A~@
              ~&       the expected result was: ~S~%"
             (getf testcase :expression)
             (getf testcase :error)
             (getf testcase :expected)))
    (otherwise
     (format t "Testcase ~A:~40T ~A~%"
             (getf testcase :identifier)
             (getf testcase :failure-type))))
  (when verbose
    (format t "~@[Data:~32T~S~%~]~@[Info:~32T~S~%~]"
	    (getf testcase :test-data)
	    (getf testcase :info))
    (format t "~@[Sysout:~%~{    ~A~%~}~]~@[Syserr:~%~{    ~A~%~}~]"
	    (split-lines (getf testcase :sysout))
	    (split-lines (getf testcase :syserr))))
  (finish-output))

(defun print-testsuites-results (suites &key (verbose *verbose*)
                                      ((:output *standard-output*) *standard-output*))
  "When VERBOSE is NIL don't report the successes."
  (dolist (suite (getf suites :suites))
    (format t "~%Testsuite ~A:~%" (getf suite :identifier))
    (dolist (testcase (getf suite :testcases))
      (print-testcase-result testcase :verbose verbose)))
  (finish-output))

(defmethod format-results ((format (eql :text)) suites-results)
  (with-output-to-string (*standard-output*)
    (print-testsuites-results suites-results)))


;;; ========================================
;;; JUNIT format
;;; ========================================

;; Note: gitlab-ci still doesn't display the testsuites and testsuite names,
;; and neither is the URL built from the [[ATTACHMENT]] path valid for an
;; artifact download or other.
;;
;; We'll ignore the path for now, and compensate for the missing
;; testuite name, by prepending it to the testcase name.

(defvar *suite-name* "" "Current testsuite name.")


(defun junit-format-testcase (testcase)
  (cl-who:with-html-output-to-string (*standard-output* nil :indent nil)
      ;; <!-- testcase can appear multiple times, see /testsuites/testsuite@tests -->
      ;; <testcase name=""       <!-- Name of the test method, required. -->
	  ;;       assertions="" <!-- number of assertions in the test case. optional. not supported by maven surefire. -->
	  ;;       classname=""  <!-- Full class name for the class the test method is in. required -->
	  ;;       status=""     <!-- optional. not supported by maven surefire. -->
	  ;;       time=""       <!-- Time taken (in seconds) to execute the test. optional -->
	  ;;       >
      ;;
      ;;   <!-- If the test was not executed or failed, you can specify one of the skipped, error or failure elements. -->
      ;;
      ;;   <!-- skipped can appear 0 or once. optional -->
      ;;   <skipped message=""   <!-- message/description string why the test case was skipped. optional -->
	  ;;   />
      ;;
      ;;   <!-- error indicates that the test errored.
      ;;        An errored test had an unanticipated problem.
      ;;        For example an unchecked throwable (exception), crash or a problem with the implementation of the test.
      ;;        Contains as a text node relevant data for the error, for example a stack trace. optional -->
      ;;   <error message="" <!-- The error message. e.g., if a java exception is thrown, the return value of getMessage() -->
	  ;;      type=""    <!-- The type of error that occured. e.g., if a java execption is thrown the full class name of the exception. -->
	  ;;      >error description</error>
      ;;
      ;;   <!-- failure indicates that the test failed.
      ;;        A failure is a condition which the code has explicitly failed by using the mechanisms for that purpose.
      ;;        For example via an assertEquals.
      ;;        Contains as a text node relevant data for the failure, e.g., a stack trace. optional -->
      ;;   <failure message="" <!-- The message specified in the assert. -->
	  ;;        type=""    <!-- The type of the assert. -->
	  ;;        >failure description</failure>
      ;;
      ;;   <!-- Data that was written to standard out while the test was executed. optional -->
      ;;   <system-out>STDOUT text</system-out>
      ;;
      ;;   <!-- Data that was written to standard error while the test was executed. optional -->
      ;;   <system-err>STDERR text</system-err>
      ;; </testcase>
	  (cl-who:htm
	   (:testcase
	    :name       (format nil "~A.~A" *suite-name* (getf testcase :identifier))
        :assertions (prin1-to-string (getf testcase :assertions 0))
        :classname  (princ-to-string (getf testcase :classname ""))
        :status     (princ-to-string (or (getf testcase :status)
                                         (case (getf testcase :failure-type)
                                           ((:failure) "FAILED")
                                           ((:success) "SUCCESSFUL")
                                           (otherwise  "ABORTED"))))
        :time       (prin1-to-string (getf testcase :time 0))
        (case (getf testcase :failure-type)
          ((:success))
          ((:disabled))
          ((:skipped)
           (cl-who:htm
	    (:skipped
             :message (cl-who:escape-string (getf testcase :reason)))))
          ((:error)
           ;; <error message="" <!-- The error message. e.g., if a java exception is thrown, the return value of getMessage() -->
           ;;        type=""    <!-- The type of error that occured. e.g., if a java execption is thrown the full class name of the exception. -->
           ;;      >error description</error>
           (cl-who:htm
		    (:error
		     :message (cl-who:escape-string (princ-to-string (getf testcase :error)))
		     :type    (cl-who:escape-string (prin1-to-string (class-of (getf testcase :error))))
		     (cl-who:esc (with-output-to-string (out)
                           (print-testcase-result testcase :output out))))))
          ((:failure)
           ;; <failure message="" <!-- The message specified in the assert. -->
	       ;;          type=""    <!-- The type of the assert. -->
	       ;;      >failure description</failure>
           (cl-who:htm
		    (:failure
		     :message (cl-who:escape-string (getf testcase :info))
		     :type    (cl-who:escape-string (prin1-to-string (or (getf testcase :test-func) (getf testcase :equal))))
		     (cl-who:esc (with-output-to-string (out)
                           (print-testcase-result testcase :output out))))))
          (otherwise))
        ;;
        ;; Testcase output (testsuite have their own system-out/system-err
        ;;
        ;; <!-- Data that was written to standard out while the test was executed. optional -->
        ;; <system-out>STDOUT text</system-out>
        (cl-who:htm (:system-out (cl-who:esc (getf testcase :sysout))))
        ;; <!-- Data that was written to standard error while the test was executed. optional -->
        ;; <system-err>STDERR text</system-err>
        (cl-who:htm (:system-err (cl-who:esc (getf testcase :syserr))))))))



(defvar *suite-id* 0)

(defun prepare-testsuite-name (name)
  (loop
    :for (new old) :in '((#\_ #\-) (#\. #\:))
    :for changed := (substitute new old (string-downcase name))
      :then (substitute new old changed)
    :finally (return changed)))

(defun junit-format-testsuite (suite)
  (let ((*suite-name* (prepare-testsuite-name (princ-to-string (getf suite :identifier)))))
    (cl-who:with-html-output-to-string (*standard-output* nil :indent nil)
	  (cl-who:htm
       ;; <!-- testsuite can appear multiple times, if contained in a testsuites element.
       ;;      It can also be the root element. -->
       ;; <testsuite name=""      <!-- Full (class) name of the test for non-aggregated testsuite documents.
       ;;                              Class name without the package for aggregated testsuites documents. Required -->
       ;;   tests=""     <!-- The total number of tests in the suite, required. -->
       ;;   disabled=""  <!-- the total number of disabled tests in the suite. optional. not supported by maven surefire. -->
       ;;   errors=""    <!-- The total number of tests in the suite that errored. An errored test is one that had an unanticipated problem,
       ;;                     for example an unchecked throwable; or a problem with the implementation of the test. optional -->
       ;;   failures=""  <!-- The total number of tests in the suite that failed. A failure is a test which the code has explicitly failed
       ;;                     by using the mechanisms for that purpose. e.g., via an assertEquals. optional -->
       ;;   hostname=""  <!-- Host on which the tests were executed. 'localhost' should be used if the hostname cannot be determined. optional.
       ;;                     not supported by maven surefire. -->
       ;;   id=""        <!-- Starts at 0 for the first testsuite and is incremented by 1 for each following testsuite. optional. not supported by maven surefire. -->
       ;;   package=""   <!-- Derived from testsuite/@name in the non-aggregated documents. optional. not supported by maven surefire. -->
       ;;   skipped=""   <!-- The total number of skipped tests. optional -->
       ;;   time=""      <!-- Time taken (in seconds) to execute the tests in the suite. optional -->
       ;;   timestamp="" <!-- when the test was executed in ISO 8601 format (2014-01-21T16:17:18). Timezone may not be specified. optional.
       ;;                     not supported by maven surefire. -->
       ;;   >
	   (:testsuite
	    :id        (prin1-to-string (incf *suite-id*))
	    :name      *suite-name*
        :package   (getf suite :package)
        :tests     (prin1-to-string (getf suite :tests    0))
        :disabled  (prin1-to-string (getf suite :disabled 0))
        :errors    (prin1-to-string (getf suite :errors   0))
        :failures  (prin1-to-string (getf suite :failures 0))
        :skipped   (prin1-to-string (getf suite :skipped  0))
        :time      (prin1-to-string (getf suite :time     0))
        :timestamp (getf suite :timestamp)
        :hostname  (getf suite :hostname "localhost")

        (unless *junit-no-properties*
          ;; <!-- Properties (e.g., environment settings) set during test execution.
          ;;      The properties element can appear 0 or once. -->
          ;; <properties>
          ;;   <!-- property can appear multiple times. The name and value attributres are required. -->
          ;;   <property name="" value=""/>
          ;; </properties>
          (cl-who:htm
           (:properties
            (:property :name "LISP-IMPLEMENTATION-TYPE"    :value (cl-who:escape-string (lisp-implementation-type)))
            (:property :name "LISP-IMPLEMENTATION-VERSION" :value (cl-who:escape-string (lisp-implementation-version)))
            (:property :name "SOFTWARE-TYPE"               :value (cl-who:escape-string (software-type)))
            (:property :name "SOFTWARE-VERSION"            :value (cl-who:escape-string (software-version)))
            (:property :name "MACHINE-INSTANCE"            :value (cl-who:escape-string (machine-instance)))
            (:property :name "MACHINE-TYPE"                :value (cl-who:escape-string (machine-type)))
            (:property :name "MACHINE-VERSION"             :value (cl-who:escape-string (machine-version)))
            (:property :name "*FEATURES*"                  :value (cl-who:escape-string (prin1-to-string *features*))))))

        (dolist (testcase (getf suite :testcases))
          (write-string (junit-format-testcase testcase))))))))

(defun junit-format-testsuites (suites)
  (cl-who:with-html-output-to-string (*standard-output* nil :indent nil)
    ;; <testsuites disabled="" <!-- total number of disabled tests from all testsuites. -->
    ;;         errors=""   <!-- total number of tests with error result from all testsuites. -->
    ;;         failures="" <!-- total number of failed tests from all testsuites. -->
    ;;         name=""
    ;;         tests=""    <!-- total number of tests from all testsuites. Some software may expect to only see the number of successful tests from all testsuites though. -->
    ;;         time=""     <!-- time in seconds to execute all test suites. -->
    ;;     >
	(cl-who:htm
     (:testsuites
      :id   (prepare-testsuite-name (getf suites :name))
      :name (prepare-testsuite-name (getf suites :name))
      :disabled (prin1-to-string (getf suites :disabled  0))
      :error    (prin1-to-string (getf suites :errors    0))
      :failures (prin1-to-string (getf suites :failures  0))
      :tests    (prin1-to-string (getf suites :tests     0))
      :time     (prin1-to-string (getf suites :time      0))
      (let ((*suite-id* -1))
        (dolist (suite (getf suites :suites))
          (write-string (junit-format-testsuite suite))))))))

(defmethod format-results ((format (eql :junit)) suites)
  "Formats then results as Junit XML, junits only allows 3 levels nl. suites, suite and testcase.
If your identifiers are not 1 or 3 levels this wont work for you."

  (cl-who:with-html-output-to-string (*standard-output* nil :indent nil)
    "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
    (when suites
      (write-string (junit-format-testsuites suites)))))


