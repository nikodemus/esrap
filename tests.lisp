;;;;  Copyright (c) 2007-2013 Nikodemus Siivola <nikodemus@random-state.net.net>
;;;;
;;;;  Permission is hereby granted, free of charge, to any person
;;;;  obtaining a copy of this software and associated documentation files
;;;;  (the "Software"), to deal in the Software without restriction,
;;;;  including without limitation the rights to use, copy, modify, merge,
;;;;  publish, distribute, sublicense, and/or sell copies of the Software,
;;;;  and to permit persons to whom the Software is furnished to do so,
;;;;  subject to the following conditions:
;;;;
;;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;;  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;;  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;;  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;;  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :cl-user)

(defpackage :esrap-tests
  (:use :alexandria :cl :esrap :eos)
  (:shadowing-import-from :esrap "!")
  (:export #:run-tests))

(in-package :esrap-tests)

(def-suite esrap)
(in-suite esrap)

;;;; A few semantic predicates

(defun not-doublequote (char)
  (not (eql #\" char)))

(defun not-digit (char)
  (when (find-if-not #'digit-char-p char)
    t))

(defun not-newline (char)
  (not (eql #\newline char)))

(defun not-space (char)
  (not (eql #\space char)))

;;;; Utility rules

(defrule whitespace (+ (or #\space #\tab #\newline))
  (:text t))

(defrule empty-line #\newline
  (:constant ""))

(defrule non-empty-line (and (+ (not-newline character)) (? #\newline))
  (:destructure (text newline)
    (declare (ignore newline))
    (text text)))

(defrule line (or empty-line non-empty-line)
  (:identity t))

(defrule trimmed-line line
  (:lambda (line)
    (string-trim '(#\space #\tab) line)))

(defrule trimmed-lines (* trimmed-line)
  (:identity t))

(defrule digits (+ (digit-char-p character))
  (:text t))

(defrule integer (and (? whitespace)
                      digits
                      (and (? whitespace) (or (& #\,) (! character))))
  (:destructure (whitespace digits tail)
    (declare (ignore whitespace tail))
    (parse-integer digits)))

(defrule list-of-integers (+ (or (and integer #\, list-of-integers)
                                 integer))
  (:destructure (match)
    (if (integerp match)
        (list match)
        (destructuring-bind (int comma list) match
          (declare (ignore comma))
          (cons int list)))))

(test smoke
  (is (equal '("1," "2," "" "3," "4.")
             (parse 'trimmed-lines "1,
                                    2,

                                    3,
                                    4.")))
  (is (eql 123 (parse 'integer "  123")))
  (is (eql 123 (parse 'integer "  123  ")))
  (is (eql 123 (parse 'integer "123  ")))
  (is (equal '(123 45 6789 0) (parse 'list-of-integers "123, 45  ,   6789, 0")))
  (is (equal '(123 45 6789 0) (parse 'list-of-integers "  123 ,45,6789, 0  "))))

(defrule single-token/bounds.1 (+ (not-space character))
  (:lambda (result &bounds start end)
    (format nil "~A[~S-~S]" (text result) start end)))

(defrule single-token/bounds.2 (and (not-space character) (* (not-space character)))
  (:destructure (first &rest rest &bounds start end)
    (format nil "~C~A(~S-~S)" first (text rest) start end)))

(defrule tokens/bounds.1 (and (? whitespace)
                              (or (and single-token/bounds.1 whitespace tokens/bounds.1)
                                  single-token/bounds.1))
  (:destructure (whitespace match)
    (declare (ignore whitespace))
    (if (stringp match)
        (list match)
        (destructuring-bind (token whitespace list) match
          (declare (ignore whitespace))
          (cons token list)))))

(defrule tokens/bounds.2 (and (? whitespace)
                              (or (and single-token/bounds.2 whitespace tokens/bounds.2)
                                  single-token/bounds.2))
  (:destructure (whitespace match)
    (declare (ignore whitespace))
    (if (stringp match)
        (list match)
        (destructuring-bind (token whitespace list) match
          (declare (ignore whitespace))
          (cons token list)))))

(defrule left-recursion (and left-recursion "l"))

(test bounds.1
  (is (equal '("foo[0-3]")
             (parse 'tokens/bounds.1 "foo")))
  (is (equal '("foo[0-3]" "bar[4-7]" "quux[11-15]")
             (parse 'tokens/bounds.1 "foo bar    quux"))))

(test bounds.2
  (is (equal '("foo(0-3)")
             (parse 'tokens/bounds.2 "foo")))
  (is (equal '("foo(0-3)" "bar(4-7)" "quux(11-15)")
             (parse 'tokens/bounds.2 "foo bar    quux"))))

(test condition.1
  "Test signaling of `esrap-simple-parse-error' conditions for failed
   parses."
  (macrolet
      ((signals-esrap-error ((input position &optional messages) &body body)
         `(progn
            (signals (esrap-error)
              ,@body)
            (handler-case (progn ,@body)
              (esrap-error (condition)
                (is (string= (esrap-error-text condition) ,input))
                (is (= (esrap-error-position condition) ,position))
                ,@(when messages
                    `((let ((report (princ-to-string condition)))
                        ,@(mapcar (lambda (message)
                                    `(is (search ,message report)))
                                  (ensure-list messages))))))))))
    (signals-esrap-error ("" 0 ("Could not parse subexpression"
                                "Encountered at"))
      (parse 'integer ""))
    (signals-esrap-error ("123foo" 3 ("Could not parse subexpression"
                                      "Encountered at"))
      (parse 'integer "123foo"))
    (signals-esrap-error ("1, " 1 ("Incomplete parse."
                                   "Encountered at"))
      (parse 'list-of-integers "1, "))))

(test condition.2
  "Test signaling of `left-recursion' condition."
  (signals (left-recursion)
    (parse 'left-recursion "l"))
  (handler-case (parse 'left-recursion "l")
    (left-recursion (condition)
      (is (string= (esrap-error-text condition) "l"))
      (is (= (esrap-error-position condition) 0))
      (is (eq (left-recursion-nonterminal condition)
              'left-recursion))
      (is (equal (left-recursion-path condition)
                 '(left-recursion left-recursion))))))

(test negation
  "Test negation in rules."
  (let* ((text "FooBazBar")
         (t1c (text (parse '(+ (not "Baz")) text :junk-allowed t)))
         (t1e (text (parse (identity '(+ (not "Baz"))) text :junk-allowed t)))
         (t2c (text (parse '(+ (not "Bar")) text :junk-allowed t)))
         (t2e (text (parse (identity '(+ (not "Bar"))) text :junk-allowed t)))
         (t3c (text (parse '(+ (not (or "Bar" "Baz"))) text :junk-allowed t)))
         (t3e (text (parse (identity '(+ (not (or "Bar" "Baz")))) text :junk-allowed t))))
    (is (equal "Foo" t1c))
    (is (equal "Foo" t1e))
    (is (equal "FooBaz" t2c))
    (is (equal "FooBaz" t2e))
    (is (equal "Foo" t3c))
    (is (equal "Foo" t3e))))

(declaim (special *depth*))
(defvar *depth* nil)

(defrule around/inner
    (+ (alpha-char-p character))
  (:text t))

(defrule around.1
    (or around/inner
        (and #\{ around.1 #\}))
  (:lambda (thing)
    (if (stringp thing)
        (cons *depth* thing)
        (second thing)))
  (:around ()
    (let ((*depth* (if *depth*
                       (cons (1+ (first *depth*)) *depth*)
                       (list 0))))
      (call-transform))))

(defrule around.2
    (or around/inner
        (and #\{ around.2 #\}))
  (:lambda (thing)
    (if (stringp thing)
        (cons *depth* thing)
        (second thing)))
  (:around (&bounds start end)
    (let ((*depth* (if *depth*
                       (cons (cons (1+ (car (first *depth*))) (cons start end))
                             *depth*)
                       (list (cons 0 (cons start end))))))
      (call-transform))))

(test around.1
  "Test executing code around the transform of a rule."
  (macrolet ((test-case (input expected)
               `(is (equal (parse 'around.1 ,input) ,expected))))
    (test-case "foo"     '((0) . "foo"))
    (test-case "{bar}"   '((1 0) . "bar"))
    (test-case "{{baz}}" '((2 1 0) . "baz"))))

(test around.2
  "Test executing code around the transform of a rule."
  (macrolet ((test-case (input expected)
               `(is (equal (parse 'around.2 ,input) ,expected))))
    (test-case "foo"     '(((0 . (0 . 3)))
                           . "foo"))
    (test-case "{bar}"   '(((1 . (1 . 4))
                            (0 . (0 . 5)))
                           . "bar"))
    (test-case "{{baz}}" '(((2 . (2 . 5))
                            (1 . (1 . 6))
                            (0 . (0 . 7)))
                           . "baz"))))

(defrule character-range (character-ranges (#\a #\b) #\-))

(test character-range-test
  (is (equal '(#\a #\b) (parse '(* (character-ranges (#\a #\z) #\-)) "ab" :junk-allowed t)))
  (is (equal '(#\a #\b) (parse '(* (character-ranges (#\a #\z) #\-)) "ab1" :junk-allowed t)))
  (is (equal '(#\a #\b #\-) (parse '(* (character-ranges (#\a #\z) #\-)) "ab-" :junk-allowed t)))
  (is (not (parse '(* (character-ranges (#\a #\z) #\-)) "AB-" :junk-allowed t)))
  (is (not (parse '(* (character-ranges (#\a #\z) #\-)) "ZY-" :junk-allowed t)))
  (is (equal '(#\a #\b #\-) (parse '(* character-range) "ab-cd" :junk-allowed t))))

(test examples-from-readme-test
  (is (equal '("foo" nil)
             (multiple-value-list (parse '(or "foo" "bar") "foo"))))
  (is (eq 'foo+ (add-rule 'foo+
                          (make-instance 'rule :expression '(+ "foo")))))
  (is (equal '(("foo" "foo" "foo") nil)
             (multiple-value-list (parse 'foo+ "foofoofoo"))))
  (is (eq 'decimal
          (add-rule 'decimal
                    (make-instance 'rule
                                   :expression `(+ (or "0" "1" "2" "3" "4" "5" "6" "7"
                                                       "8" "9"))
                                   :transform (lambda (list start end)
                                                (declare (ignore start end))
                                                (parse-integer (format nil "~{~A~}" list)))))))
  (is (eql 123 (parse '(oddp decimal) "123")))
  (is (equal '(nil 0)
             (multiple-value-list (parse '(evenp decimal) "123" :junk-allowed t)))))

;; Testing ambiguity when repetitioning possibly empty-string-match

(defrule spaces (* #\space)
  (:lambda (lst)
    (length lst)))

(defrule greedy-pos-spaces (+ spaces))
(defrule greedy-spaces (* spaces))

(test ambiguous-greedy-repetitions
  (is (equal '((3) nil) (multiple-value-list (parse 'greedy-spaces "   "))))
  (is (equal '((3) nil) (multiple-value-list (parse 'greedy-pos-spaces "   ")))))

(defparameter separator #\space)

(defrule simple-prefix (character-ranges (#\a #\z)))

(defun separator-p (x)
  (and (characterp x) (char= x separator)))

(defrule separator (separator-p character))

(defrule word (+ (not separator))
  (:text t))

(defrule simple-wrapped (wrap simple-prefix
			      (and word
				   (* (and separator word))
				   (? separator)))
  (:wrap-around (let ((separator wrapper))
		  (call-parser)))
  (:destructure (word rest-words sep)
		(declare (ignore sep))
		`(,word ,@(mapcar #'cadr rest-words))))

(test dynamic-wrapping			      
  (is (equal '(("oo" "oo" "oo") nil)
	     (multiple-value-list (parse 'simple-wrapped "foofoofoof"))))
  (is (equal '(("oofoofoof") nil)
	     (multiple-value-list (parse 'simple-wrapped "goofoofoof")))))

(defparameter dyna-from 3)
(defparameter dyna-to 5)

(defrule dyna-from-to (* dyna-from dyna-to "a")
  (:text t))

(defrule dyna-from-tos (* dyna-from-to))

(test dynamic-times
  (is (equal '("aaaaa" "aaa") (parse 'dyna-from-tos "aaaaaaaa")))
  (is (equal '("aaaa" "aaaa") (let ((dyna-to 4))
				(parse 'dyna-from-tos "aaaaaaaa")))))

(defrule cond-word (cond (dyna-from-to word)))

(defparameter context nil)
(defun in-context-p (x)
  (declare (ignore x))
  context)
(defrule context (in-context-p ""))
(defrule ooc-word word
  (:constant "out of context word"))
(test cond
  (is (equal "foo" (parse 'cond-word "aaaafoo")))
  (is (equal "foo" (let ((context t)) (parse '(cond (context word)) "foo"))))
  (is (equal :error-occured (handler-case (parse '(cond (context word)) "foo")
			      (error () :error-occured))))
  (is (equal "out of context word" (parse '(cond (context word) (t ooc-word))
					  "foo"))))

(defun run-tests ()
  (let ((results (run 'esrap)))
    (eos:explain! results)
    (unless (eos:results-status results)
      (error "Tests failed."))))
