all: test documentation
.PHONY:: test tests documentation
test tests:
	sbcl --noinform --no-userinit --non-interactive \
		--eval '(load #P"~/quicklisp/setup.lisp")' \
		--eval '(push "$(DEPENDENCYDIR)" ql:*local-project-directories*)' \
		--eval '(push #P"./" asdf:*central-registry*)' \
		--eval '(ql:quickload :cl-naive-tests)' \
		--eval '(ql:quickload :cl-naive-tests.tests)' \
		--eval '(defparameter cl-naive-tests.tests::*disable-failure-tests* t)' \
		--eval '(cl-naive-tests:run)' \
		--eval '(cl-naive-tests:run)' \
		--eval '(cl-naive-tests:write-results cl-naive-tests:*suites-results* :format :text)' \
		--eval '(cl-naive-tests:save-results cl-naive-tests:*suites-results* :file "/root/src/junit-results.xml" :format :junit)' \
		--eval '(sb-ext:exit :code (if (cl-naive-tests:report) 0 200))'
documentation:
	make -C docs pdfs

