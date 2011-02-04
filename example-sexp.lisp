;;;; Esrap example: a simple S-expression grammar

(require :esrap)

(defpackage :sexp-grammar
  (:use :cl :esrap))

(in-package :sexp-grammar)

(defun whitespacep (char)
  (member char '(#\space #\tab #\newline)))

(defun string-charp (char)
  (not (eql #\" char)))

(defrule whitespace (whitespacep character)
  (:constant nil))

(defrule whitespace* (* whitespace)
  (:constant nil))

(defrule whitespace+ (+ whitespace)
  (:constant nil))

(defrule sexp (or list atom))

(defrule list (and #\( whitespace* sexp (* (and whitespace+ sexp)) whitespace* #\))
  (:destructure (p1 w1 car cdrs w2 p2)
    (declare (ignore p1 p2 w1 w2))
    (cons car (mapcar #'second cdrs))))

(defrule atom (or string integer symbol))

(defrule string (and #\" (* (or (string-charp character) (and #\\ #\"))) #\")
  (:destructure (q1 string q2)
    (declare (ignore q1 q2))
    (esrap:concat string)))

(defrule integer (+ (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
  (:lambda (list)
    (parse-integer (esrap:concat list) :radix 10)))

(defrule symbol (+ alpha)
  (:lambda (list)
    (intern (esrap:concat list))))

(defrule alpha (alphanumericp character))

;;;; Try these

(parse 'sexp "FOO123")

(parse 'sexp "123")

(parse 'sexp "\"foo\"")

(parse 'sexp "(  1 2  3 (FOO \"foo\" 123 )   )")

(describe-grammar 'sexp)
