;;;; Esrap example: a simple S-expression grammar

(defpackage :xml-sexp-grammar
  (:use :cl :esrap))

(in-package :xml-sexp-grammar)

;;; A semantic predicate for filtering out double quotes.

(defun not-doublequote (char)
  (not (eql #\" char)))

(defun not-integer (string)
  (when (find-if-not #'digit-char-p string)
    t))

(defvar *sexp-grammar* (make-grammar))

;;; Utility rules.

(defrule *sexp-grammar* whitespace (+ (or #\space #\tab #\newline))
  (:constant nil))

(defrule *sexp-grammar* alphanumeric (alphanumericp character))

(defrule *sexp-grammar* string-char (or (not-doublequote character) (and #\\ #\")))

;;; Here we go: an S-expression is either a list or an atom, with possibly leading whitespace.

(defrule *sexp-grammar* sexp (and (? whitespace) (or magic list atom))
  (:destructure (w s)
    (declare (ignore w))
    s))

(defrule *sexp-grammar* magic "foobar"
  (:constant :magic)
  (:when (eq * :use-magic)))

(defrule *sexp-grammar* list (and #\( sexp (* sexp) (? whitespace) #\))
  (:destructure (p1 car cdr w p2)
    (declare (ignore p1 p2 w))
    (cons car cdr)))

(defrule *sexp-grammar* atom (or string integer symbol))

(defrule *sexp-grammar* string (and #\" (* string-char) #\")
  (:destructure (q1 string q2)
    (declare (ignore q1 q2))
    (text string)))

(defrule *sexp-grammar* integer (+ (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
  (:lambda (list)
    (parse-integer (text list) :radix 10)))

(defrule *sexp-grammar* symbol (not-integer (+ alphanumeric))
  ;; NOT-INTEGER is not strictly needed because ATOM considers INTEGER before
  ;; a STRING, we know can accept all sequences of alphanumerics -- we already
  ;; know it isn't an integer.
  (:lambda (list)
    (intern (text list))))

;;;; Try these

(parse *sexp-grammar* 'sexp "FOO123")

(parse *sexp-grammar* 'sexp "123")

(parse *sexp-grammar* 'sexp "\"foo\"")

(parse *sexp-grammar* 'sexp "  (  1 2  3 (FOO\"foo\"123 )   )")

(parse *sexp-grammar* 'sexp "foobar")

(let ((* :use-magic))
  (parse *sexp-grammar* 'sexp "foobar"))

(describe-grammar *sexp-grammar* 'sexp)

(trace-rule *sexp-grammar* 'sexp :recursive t)

(parse *sexp-grammar* 'sexp "(foo bar 1 quux)")

(untrace-rule *sexp-grammar* 'sexp :recursive t)

(defparameter *orig* (rule-expression (find-rule *sexp-grammar* 'sexp)))

(change-rule *sexp-grammar* 'sexp '(and (? whitespace) (or list symbol)))

(parse *sexp-grammar* 'sexp "(foo bar quux)")

(parse *sexp-grammar* 'sexp "(foo bar 1 quux)" :junk-allowed t)

(change-rule *sexp-grammar* 'sexp *orig*)

(parse *sexp-grammar* 'sexp "(foo bar 1 quux)" :junk-allowed t)

;; "XML" is starting

(defvar *xml-grammar* (make-grammar))

(defrule *xml-grammar* whitespace (+ (or #\space #\tab #\newline))
  (:constant nil))

(defrule *xml-grammar* string-char (or (not-doublequote character) (and #\\ #\")))

(defrule *xml-grammar* string (and #\" (* string-char) #\")
  (:destructure (q1 string q2)
    (declare (ignore q1 q2))
    (text string)))

(defrule *xml-grammar* starting-symbol (and "<" (+ (and (! ">") character)) ">")
  (:lambda (list)
    (intern (text list))))

(defrule *xml-grammar* ending-symbol (and "<" (+ (and (! ">") character)) ">")
  (:lambda (list)
    (intern (text list))))

(defrule *xml-grammar* sexp-element (and "<sexp>"
					 (+ (and (! "</sexp>") character))
					 "</sexp>")
  (:destructure (ss sexpres es)
    (declare (ignore es))
    (cons (intern (string-trim "<>" ss))
	  (parse *sexp-grammar* 'sexp (text "(" sexpres ")")))))

(defrule *xml-grammar* term-element (and starting-symbol
					 (+ (and (! ending-symbol) character))
					 ending-symbol)
  (:destructure (ss innertext es)
    (declare (ignore es))
    (list (intern (string-trim "<>" ss)) (text innertext))))

(defrule *xml-grammar* element (and starting-symbol
				    (+ (or sexp-element element term-element))
				    ending-symbol)
  (:destructure (ss innertext es)
    (declare (ignore es))
    (cons (intern (string-trim "<>" ss)) innertext)))

(defrule *xml-grammar* xml (or sexp-element element term-element))

(parse *xml-grammar* 'xml "<test><i>text</i><sexp>(a 2 3)</sexp></testy>")
