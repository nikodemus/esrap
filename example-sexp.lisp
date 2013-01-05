;;;; Esrap example: a simple S-expression grammar

(require :esrap)

(defpackage :sexp-grammar
  (:use :cl :esrap))

(in-package :sexp-grammar)

(defun not-integer (partial-result)
  (when (find-if-not #'digit-char-p (text partial-result))
    t))

(defgrammar #:sexp
  (:documentation
   "A simple grammar for S-expressions."))
(in-grammar #:sexp)

;;; Utility rules.

(defrule whitespace (+ (or #\space #\tab #\newline))
  (:constant nil))

;;; Here we go: an S-expression is either a list or an atom, with possibly leading whitespace.

(defrule sexp (and (? whitespace) (or magic list atom))
  (:destructure (w s &bounds start end)
    (declare (ignore w))
    (list s (cons start end))))

(defrule magic "foobar"
  (:constant :magic)
  (:when (eq * :use-magic)))

(defrule list (and #\( sexp (* sexp) (? whitespace) #\))
  (:destructure (p1 car cdr w p2)
    (declare (ignore p1 p2 w))
    (cons car cdr)))

(defrule string (and #\" (* (not #\")) #\")
  (:destructure (q1 string q2)
    (declare (ignore q1 q2))
    (text string)))

(defrule atom (or string integer symbol))

(defrule integer (+ (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
  (:lambda (list)
    (parse-integer (text list) :radix 10)))

(defrule symbol (not-integer (+ (or (alphanumericp character) #\< #\> #\/ #\- #\_ #\?)))
  ;; NOT-INTEGER is not strictly needed because ATOM considers INTEGER before
  ;; a STRING, we know can accept all sequences of alphanumerics -- we already
  ;; know it isn't an integer.
  (:lambda (list)
    (intern (text list))))

;;;; Try these

(find-grammar '#:sexp)

(parse 'sexp "FOO123")

(parse 'sexp "123")

(parse 'sexp "\"foo\"")

(parse 'sexp "  (  1 2  3 (FOO\"foo\"123 )   )")

(parse 'sexp "foobar")

#+no (let ((* :use-magic))
  (parse '#:sexp 'sexp "foobar"))

(describe-grammar '#:sexp 'sexp)

(trace-rule '#:sexp 'sexp :recursive t)

(parse 'sexp "(foo bar 1 quux)")

(untrace-rule '#:sexp 'sexp :recursive t)

(defparameter *orig* (rule-expression (find-rule '#:sexp 'sexp)))

(change-rule '#:sexp 'sexp '(and (? whitespace) (or list symbol)))

(parse 'sexp "(foo bar quux)")

(parse 'sexp "(foo bar 1 quux)" :junk-allowed t)

(change-rule '#:sexp 'sexp *orig*)

(parse 'sexp "(foo bar 1 quux)" :junk-allowed t)
