;;;; Esrap example: a simple S-expression grammar

(require :esrap)

(defpackage :sexp-grammar
  (:use :cl :esrap))

(in-package :sexp-grammar)

;;; A semantic predicate for filtering out double quotes.

(defun not-doublequote (char)
  (not (eql #\" char)))

(defun not-integer (string)
  (when (find-if-not #'digit-char-p string)
    t))

;;; Utility rules.

(make-grammar sexp)

(sexp-defrule whitespace (+ (or #\space #\tab #\newline))
  (:constant nil))

(sexp-defrule alphanumeric (alphanumericp character))

(sexp-defrule string-char (or (not-doublequote character) (and #\\ #\")))

;;; Here we go: an S-expression is either a list or an atom, with possibly leading whitespace.

(sexp-defrule sexp (and (? whitespace) (or magic list atom))
  (:destructure (w s &bounds start end)
    (declare (ignore w))
    (list s (cons start end))))

(sexp-defrule magic "foobar"
  (:constant :magic)
  (:when (eq * :use-magic)))

(sexp-defrule list (and #\( sexp (* sexp) (? whitespace) #\))
  (:destructure (p1 car cdr w p2)
    (declare (ignore p1 p2 w))
    (cons car cdr)))

(sexp-defrule atom (or string integer symbol))

(sexp-defrule string (and #\" (* string-char) #\")
  (:destructure (q1 string q2)
    (declare (ignore q1 q2))
    (text string)))

(sexp-defrule integer (+ (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
  (:lambda (list)
    (parse-integer (text list) :radix 10)))

(sexp-defrule symbol (not-integer (+ alphanumeric))
  ;; NOT-INTEGER is not strictly needed because ATOM considers INTEGER before
  ;; a STRING, we know can accept all sequences of alphanumerics -- we already
  ;; know it isn't an integer.
  (:lambda (list)
    (intern (text list))))

;;;; Try these

(sexp-parse 'sexp "FOO123")

(sexp-parse 'sexp "123")

(sexp-parse 'sexp "\"foo\"")

(sexp-parse 'sexp "  (  1 2  3 (FOO\"foo\"123 )   )")

(sexp-parse 'sexp "foobar")

(let ((* :use-magic))
  (sexp-parse 'sexp "foobar"))

(sexp-describe-grammar 'sexp)

(sexp-trace-rule 'sexp :recursive t)

(sexp-parse 'sexp "(foo bar 1 quux)")

(sexp-untrace-rule 'sexp :recursive t)

(defparameter *orig* (rule-expression (sexp-find-rule 'sexp)))

(sexp-change-rule 'sexp '(and (? whitespace) (or list symbol)))

(sexp-parse 'sexp "(foo bar quux)")

(sexp-parse 'sexp "(foo bar 1 quux)" :junk-allowed t)

(sexp-change-rule 'sexp *orig*)

(sexp-parse 'sexp "(foo bar 1 quux)" :junk-allowed t)
