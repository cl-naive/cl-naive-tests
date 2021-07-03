(in-package :cl-naive-tests.tests)

;;;; If you're a purist you are going to have a heart attack if you
;;;; look at this.  I am using the package I want to test to test
;;;; itself :P

(defvar *disable-failure-tests* nil)

(testsuite :test-pass
  (testcase :without-test-func
		    :expected t
			:actual t
		    :info "(equal t t)")
  (testcase :with-test-func
		    :test-func #'(lambda (data info)
				           (declare (ignore data info))
				           (equalp t t))
		    :info "(equal t t)"))

(testsuite :test-fail
  (testcase :without-test-func
            :disabled *disable-failure-tests*
		    :expected nil
			:actual t
		    :info "(equal nil t)")
  (testcase :with-test-func
            :disabled *disable-failure-tests*
		    :test-func #'(lambda (data info)
				           (declare (ignore data info))
				           (equalp nil t))
		    :info "(equalp nil t)"))


(defun plist-keys (plist)
  (loop :for (key nil) :on plist :by (function cddr)
        :collect key))

(defun set-equalp (a b)
  (and (subsetp a b) (subsetp b a)))

(defmacro show (x)
  (let ((vresult (gensym)))
   `(let ((,vresult ,x))
      (format *terminal-io* "~%if ~S -> ~S"
              ',x ,vresult)
      (finish-output *terminal-io*)
      ,vresult)))

(defun plist-tree-without-wildcards (tree wildcard-keys)
  (labels ((process-node (node)
             (if (atom node)
                 node
                 (let ((len (list-length node)))
                   (if (null len)
                       ;; dotted-list:
                       (cons (process-node (car node))
                             (process-node (cdr node)))
                       ;; proper-list:
                       (if (and (evenp len) (symbolp (car node)))
                           ;; possibly a plist
                           (mapcan (lambda (key)
                                     (list key (process-node (getf node key))))
                                   (set-difference (plist-keys node) wildcard-keys))
                           ;; a normal list
                           (cons (process-node (car node))
                                 (process-node (cdr node)))))))))
    (process-node tree)))

(defun plist-tree-equal-p (a b &key wildcard-keys)
  ;; WARNING: We don't take into account circular trees.
  (labels ((node-equal-p (a b)
             (cond
               ((eql a b) t)
               ((atom a)  (equal a b))
               (t (if (consp b)
                      (let ((la (list-length a))
                            (lb (list-length b)))
                        (if (eql la lb)
                            (if (null la)
                                ;; dotted lists:
                                (and (node-equal-p (car a) (car b))
                                     (node-equal-p (cdr a) (cdr b)))
                                ;; proper list:
                                (if (and (evenp la) (symbolp (car a)))
                                    ;; possibly a plist
                                    (let ((ka (plist-keys a))
                                          (kb (plist-keys b)))
                                      (if (set-equalp ka kb)
                                          (every (lambda (key)
                                                   (if (find key wildcard-keys)
                                                       t
                                                       (node-equal-p (getf a key) (getf b key))))
                                                 ka)
                                          nil))
                                    ;; a normal list
                                    (and (node-equal-p (car a) (car b))
                                         (node-equal-p (cdr a) (cdr b)))))
                            nil))
                      nil)))))
    (node-equal-p a b)))

(defparameter *result-wildcards* '(:time :timestamp :hostname :sysout :syserr))

(defun suites-results-equal-p (testcase info)
  (plist-tree-equal-p (getf testcase :actual)
                      (getf testcase :expected)
                      :wildcard-keys *result-wildcards*))

(testsuite :test-result
  (let* ((tested-testsuites (make-hash-table :test #'equal))
         (suites-results
           (let ((*test-suites*     tested-testsuites)
                 (*suites-results*  nil))

             (testsuite :tested-testsuite
               (testcase :eish
			             :expected t
			             :actual t
			             :info "(equal t t)")
               (testcase :hsie
				         :expected t
				         :actual t
				         :info "(equal t t)"))

             (run :debug nil :name :tested-suites))))

    (testcase :results
		      :expected '(:SUITES
                          ((:TESTCASES
                            ((:ERROR NIL :FAILURE-TYPE :SUCCESS :ACTUAL
                                     T :EXPRESSION T :EXPECTED T :TEST-DATA NIL
                              :TEST-FUNC NIL :EQUAL EQUAL :INFO
                                     "(equal t t)" :IDENTIFIER :EISH :RESULT
                                                       :SUCCESS)
                             (:ERROR NIL :FAILURE-TYPE :SUCCESS :ACTUAL
                                     T :EXPRESSION T :EXPECTED T :TEST-DATA NIL
                              :TEST-FUNC NIL :EQUAL EQUAL :INFO
                                     "(equal t t)" :IDENTIFIER :HSIE :RESULT
                                                       :SUCCESS))
                            :PACKAGE "CL-NAIVE-TESTS.TESTS" :SKIPPED 0
                            :DISABLED 0 :FAILURES 0 :ERRORS 0 :TESTS 2
                            :IDENTIFIER :TESTED-TESTSUITE))
                          :TESTS 2 :FAILURES 0 :ERRORS 0 :SKIPPED 0
                          :DISABLED 0 :NAME :TESTED-SUITES)
              :test-func 'suites-results-equal-p
			  :actual (plist-tree-without-wildcards suites-results *result-wildcards*)
		      :info "tested-testsuite results")

    (testcase :format-default
              :equal 'string=
		      :expected  "(:SUITES ((:TESTCASES ((:ERROR NIL :FAILURE-TYPE :SUCCESS :ACTUAL T :EXPRESSION T :EXPECTED T :TEST-DATA NIL :TEST-FUNC NIL :EQUAL EQUAL :INFO \"(equal t t)\" :IDENTIFIER :EISH :RESULT :SUCCESS) (:ERROR NIL :FAILURE-TYPE :SUCCESS :ACTUAL T :EXPRESSION T :EXPECTED T :TEST-DATA NIL :TEST-FUNC NIL :EQUAL EQUAL :INFO \"(equal t t)\" :IDENTIFIER :HSIE :RESULT :SUCCESS)) :PACKAGE \"CL-NAIVE-TESTS.TESTS\" :SKIPPED 0 :DISABLED 0 :FAILURES 0 :ERRORS 0 :TESTS 2 :IDENTIFIER :TESTED-TESTSUITE)) :TESTS 2 :FAILURES 0 :ERRORS 0 :SKIPPED 0 :DISABLED 0 :NAME :TESTED-SUITES)"
			  :actual (with-standard-io-syntax
                        (format-results t (plist-tree-without-wildcards suites-results *result-wildcards*)))
		      :info "tested-testsuite formatted results (default)")

    (testcase :format-junit
              :equal 'string=
		      :expected "<?xml version=\"1.0\" encoding=\"UTF-8\" ?><testsuites id='TESTED-SUITES' name='TESTED-SUITES' disabled='0' error='0' failures='0' tests='2' time='0'><testsuite id='0' name='-TESTED-TESTSUITE' tests='2' disabled='0' errors='0' failures='0' skipped='0' hostname='localhost' package='CL-NAIVE-TESTS.TESTS' time='NIL'><testcase id='EISH' name='EISH' assertions='0' classname='' status='SUCCESSFUL' time='0'></testcase><system-out></system-out><system-err></system-err><testcase id='HSIE' name='HSIE' assertions='0' classname='' status='SUCCESSFUL' time='0'></testcase><system-out></system-out><system-err></system-err></testsuite></testsuites>"
			  :actual (with-standard-io-syntax
                         (let ((cl-naive-tests:*junit-no-properties* t))
                           (format-results :junit (plist-tree-without-wildcards suites-results *result-wildcards*))))
              :info "tested-testsuite formatted results (junit)")
    ))

;;(report (run))
