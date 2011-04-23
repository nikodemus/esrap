.PHONY: clean

all: index.html

index.html: ../doc/esrap.html ../tools/analytics.script
	sbcl --script ../tools/splice-to-head.lisp ../tools/analytics.script \
	  < ../doc/esrap.html > index.html

clean:
	rm -f *~ *.lisp *.lisp.html \#*
