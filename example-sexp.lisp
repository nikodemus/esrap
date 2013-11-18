;;;; Esrap example: a simple S-expression grammar

(require :esrap-liquid)

(defpackage :sexp-grammar
  (:use :cl :esrap-liquid))

(in-package :sexp-grammar)

(enable-read-macro-tokens)

;;; A semantic predicate for filtering out double quotes.

(defun not-doublequote (char)
  (not (eql #\" char)))

(defun not-integer (string)
  (when (find-if-not #'digit-char-p string)
    t))

;;; Utility rules.

(defrule whitespace ()
  (postimes (|| #\space #\tab #\newline))
  nil)

(defrule alphanumeric ()
  (pred #'alphanumericp character))

(defrule string-char ()
  (|| (pred #'not-doublequote character)
      (list #\\ #\")))

;;; Here we go: an S-expression is either a list or an atom, with possibly leading whitespace.

(defrule sexp ()
  (? whitespace)
  (list (|| magic list atom)
        (cons match-start match-end)))

(defrule magic ()
  (if (eq * :use-magic)
      (progn "foobar"
             :magic)
      (fail-parse "No room for magic in this world")))

(defrule list ()
  #\(
  (let ((res `(,sexp ,. (times sexp))))
    (? whitespace)
    #\)
    res))

(defrule atom ()
  (|| string integer symbol))

(defrule string ()
  (text (progm #\" (times string-char) #\")))

(defrule integer ()
  (parse-integer (text (postimes (|| "0" "1" "2" "3" "4" "5" "6" "7" "8" "9")))
                 :radix 10))

(defrule symbol ()
  (intern (text (pred #'not-integer (postimes alphanumeric)))))

;;;; Try these

(parse 'sexp "FOO123")

(parse 'sexp "123")

(parse 'sexp "\"foo\"")

(parse 'sexp "  (  1 2  3 (FOO\"foo\"123 )   )")

(parse 'sexp "foobar")

(let ((* :use-magic))
  (parse 'sexp "foobar"))

;; (describe-grammar 'sexp)

;; (trace-rule 'sexp :recursive t)

(parse 'sexp "(foo bar 1 quux)")

;; (untrace-rule 'sexp :recursive t)

;; (defparameter *orig* (rule-expression (find-rule 'sexp)))

;; (change-rule 'sexp '(and (? whitespace) (or list symbol)))

(parse 'sexp "(foo bar quux)")

(parse 'sexp "(foo bar 1 quux)" :junk-allowed t)

;; (change-rule 'sexp *orig*)

(parse 'sexp "(foo bar 1 quux)" :junk-allowed t)
