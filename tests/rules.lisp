;;;; tests/rules.lisp

;;;; This is a part of esrap-liquid TDPL for Common Lisp
;;;; Alexander Popolitov, 2013
;;;; For licence, see COPYING


(in-package :esrap-liquid-tests)

(cl-interpol:enable-interpol-syntax)

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

(defrule whitespace-char ()
  (|| (v #\space) (v #\tab) (v #\newline)))

(defrule whitespace ()
  (text (postimes (v whitespace-char))))

(defrule maybe-whitespace ()
  (text (? (v whitespace))))

(defrule maybe-whitespace-char ()
  (text (? (v whitespace-char))))

(defrule empty-line ()
  (progn (v #\newline) ""))

(defrule nonewline-line ()
  (postimes (pred #'not-newline (v character))))

(defrule a-char-line ()
  (postimes (v #\a)))

(defrule maybe-newline ()
  (? (v #\newline)))

(defrule non-empty-line ()
  (text (prog1 (postimes (pred #'not-newline (v character)))
          (? (v #\newline)))))

(defrule space ()
  (v #\space))

(defrule line ()
  (|| (v empty-line) (v non-empty-line)))

(defrule trimmed-line ()
  (string-trim '(#\space #\tab) (v line)))

(defrule trimmed-lines ()
  (times (v trimmed-line)))

(defrule digits ()
  (text (postimes (pred #'digit-char-p (v character)))))

(defrule integer ()
  (parse-integer (progm (? (v whitespace))
                        (v digits)
                        (list (? (v whitespace)) (|| (& (v #\,)) (! (v character)))))))

(defrule list-of-integers ()
  (let ((it (|| (list (v integer)
		      (v #\,)
		      (progn ;; (format t (literal-string "I'm here!~%"))
			     (esrap-liquid::print-iter-state)
			     (v list-of-integers)))
                (v integer))))
    (if (integerp it)
        (list it)
        (destructuring-bind (int comma list) it
          (declare (ignore comma))
          (cons int list)))))

(defrule single-token/bounds.1 ()
  (format nil "~A[~S-~S]"
          (text (postimes (pred #'not-space (v character))))
          match-start
          match-end))

(defrule single-token/bounds.2 ()
  (format nil "~C~A(~S-~S)"
          (pred #'not-space (v character))
          (text (times (pred #'not-space (v character))))
          match-start
          match-end))

(defrule tokens/bounds.1 ()
  (let ((match (progn (? (v whitespace))
                      (|| (cons (v single-token/bounds.1)
                                (progn (v whitespace) (v tokens/bounds.1)))
                          (v single-token/bounds.1)))))
    (if (stringp match)
        (list match)
        match)))

(defrule tokens/bounds.2 ()
  (let ((match (progn (? (v whitespace))
                      (|| (cons (v single-token/bounds.2)
                                (progn (v whitespace) (v tokens/bounds.2)))
                          (v single-token/bounds.2)))))
    (if (stringp match)
        (list match)
        match)))

(defrule left-recursion ()
  (progn (v left-recursion) (v "l")))

(declaim (special *depth*))
(defvar *depth* nil)

(defrule around/inner ()
  (text (postimes (pred #'alpha-char-p (v character)))))

(defrule around.1 ()
  (let ((*depth* (if *depth*
                     (cons (1+ (first *depth*)) *depth*)
                     (list 0))))
    (let ((it (|| (v around/inner)
                  (list (v #\{) (v around.1) (v #\})))))
      (if (stringp it)
          (cons *depth* it)
          (second it)))))

(defrule around.2 ()
  (let ((it (|| (v around/inner)
                (progm (v #\{) (v around.2) (v #\})))))
    (if (stringp it)
        `(((0 . (,match-start . ,match-end))) . ,it)
        `(((,(1+ (caaar it)) . (,match-start . ,match-end)) ,. (car it)) . ,(cdr it)))))

(defrule character-range ()
  (character-ranges (#\a #\b) #\-))

;; ;; Testing ambiguity when repetitioning possibly empty-string-match

(defrule spaces ()
  (length (times (v #\space))))

(defrule three-spaces ()
  (length (times (v #\space) :exactly 3)))

(defrule upto-three-spaces ()
  (length (times (v #\space) :upto 3)))

(defrule greedy-pos-spaces ()
  (postimes (v spaces)))
(defrule greedy-spaces ()
  (times (v spaces)))

;; Subtle bug here was caused by the fact, that SEPARATOR variable name
;; was the same as SEPARATOR rule-name.
(defparameter separator-char #\space)

(defrule simple-prefix ()
  (character-ranges (#\a #\z)))

(defun separator-p (x)
  (and (characterp x) (char= x separator-char)))

(defrule separator ()
  (pred #'separator-p (v character)))

(defrule not-separator ()
  (!! (v separator)))

(defrule word ()
  (text (postimes (!! (v separator)))))

(defrule simple-wrapped ()
  (let ((separator-char (v simple-prefix)))
    (prog1 (cons (v word)
                 (times (progn (v separator) (v word))))
      (? (v separator)))))

(defparameter dyna-from-times 3)
(defparameter dyna-to-times 5)

(defrule dyna-from-to ()
  (text (times (v "a") :from dyna-from-times :upto dyna-to-times)))

(defrule dyna-from-tos ()
  (times (v dyna-from-to)))

(defparameter context :void)

(defun in-context-p (x)
  (declare (ignore x))
  (and context (not (eql context :void))))

(defrule context-sensitive ()
  (pred #'in-context-p (v "")))
(defrule ooc-word ()
  (v word)
  "out of context word")

(defrule cond-word ()
  (v dyna-from-to)
  (v word))

(defrule foo+ ()
  (postimes (v "foo")))

(defrule bar+ ()
  (postimes (v "bar")))

(defrule decimal ()
  (parse-integer (format nil "~{~A~}"
                         (postimes (|| (v "0") (v "1") (v "2") (v "3") (v "4")
				       (v "5") (v "6") (v "7") (v "8") (v "9"))))))

;;; Here we test correctness of defininion of parsing environments

(define-foo-rule abracadabra ()
  "foo")

(define-bar-rule abracadabra ()
  "bar")

(let ((map '((#\a . :a) (#\b . :b) (#\c . :c))))
  (defrule closure-rule ()
    (cdr (assoc (character-ranges (#\a #\c))
		map))))

(defrule dressed-elegantly ()
  (v "bar") (v "bar") (v "bar") (cap a foo+) (v "bar") (v "bar") (v "bar")
  (recap a))

(defrule dressed-elegantly-2 ()
  (|| (progn (v "bar") (v "bar") (v "bar") (cap a foo+) (v "bar") (v "bar") (v "bar"))
      (progn (v "bar") (v "bar") (cap a foo+) (v "bar") (v "bar")))
  (recap a))

(defrule cap-overwrite ()
  (cap a bar+) (cap b foo+) (cap b bar+)
  (list (recap a) (recap b)))

(defrule f-opt-times (&optional (n 3))
  (times (v "f") :exactly n))


(defrule cipher ()
  (character-ranges (#\0 #\9)))

(defrule recurcapturing ()
  (v #\() (|| (progn (cap int cipher) (cap rc recurcapturing))
	      (v #\a))
  (v #\))
  (cons (recap int) (recap rc)))

(defrule triple-a ()
  (list (v #\a) (v #\a) (v #\a)))

(defrule abc-or-def ()
  (|| (list (v #\a) (v #\b) (v #\c))
      (list (v #\d) (v #\e) (v #\f))))

