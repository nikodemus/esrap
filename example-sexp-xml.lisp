;;;; Esrap example: mix pseudo-XML and a simple S-expression grammar

(require :esrap)

(defpackage :xml-sexp-grammar
  (:use :cl :esrap :sexp-grammar))

(in-package :xml-sexp-grammar)

(defgrammar #:xml
  (:use #:sexp)
  (:documentation
   "Accepts pseudo-XML mixed with S-expressions as defined in the SEXP
grammar."))
(in-grammar #:xml)

(defrule open-tag (and "<" (and (! "sexp") (+ (not ">"))) ">")
  (:destructure (open name close)
    (declare (ignore open close))
    (intern (text name))))

(defrule close-tag (and "</" (and (! "sexp") (+ (not ">"))) ">")
  (:destructure (open name close)
    (declare (ignore open close))
    (intern (text name))))

(defrule sexp-element (and "<sexp>" sexp "</sexp>")
  (:function second))

(defrule term-element (and open-tag
                           (and (not "<") (* (and (! close-tag) character)))
                           close-tag)
  (:destructure (open innertext close)
    (assert (string= open close))
    (list open (text innertext))))

(defrule element (and open-tag
                      (+ (or sexp-element term-element element))
                      close-tag)
  (:destructure (open content close)
    (assert (string= open close))
    (cons open content)))

(defrule xml (or sexp-element element term-element))

;;;; Try these

(parse 'xml "<test><i>text</i><sexp>(a 2 3 foo </sexp> \"bla</sexp><foo><foo/>)\")</sexp></test>")
