# -*- mode:makefile-gmake; coding:utf-8; indent-tabs-mode:t; tab-width:4 -*-

NAME=cl-naive-tests--docs
PDFS=$(NAME).pdf
all:documents
	@if [ $$(uname) = Darwin ] ; then open $(PDFS) ; else xpdf $(PDFS) ; fi

.PHONY:: documents pdfs

documents pdfs :$(PDFS)

cl-naive-tests--docs.pdf:docs.pdf
	- mv $^ $@

# Generate PDF from org-mode document.
%.pdf:%.org
	-rm -f $@
	emacs --batch \
		--eval '(find-file "'$<'")' \
		--funcall org-latex-export-to-pdf \
		--kill
clean::
	- rm -f $(NAME).tex missfont.log ./--version.lock
