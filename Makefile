all: test documentation
.PHONY:: test tests documentation
test tests:
	sbcl --noinform --no-userinit --non-interactive \
		--eval '(load #P"~/quicklisp/setup.lisp")' \
		--eval '(push #P"./" asdf:*central-registry*)' \
		--eval '(ql:quickload :cl-naive-tests.tests)' \
		--eval '(sb-ext:exit :code (if (cl-plist-utils.tests:report (cl-.tests:run)) 0 200))'
documentation:
	make -C docs pdfs

