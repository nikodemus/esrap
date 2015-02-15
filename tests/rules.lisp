;;;; tests/rules.lisp

;;;; This is a part of esrap-liquid TDPL for Common Lisp
;;;; Alexander Popolitov, 2013
;;;; For licence, see COPYING


(in-package :esrap-liquid-tests)

(enable-read-macro-tokens)
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
  (|| #\space #\tab #\newline))

(defrule whitespace ()
  (text (postimes whitespace-char)))

(defrule maybe-whitespace ()
  (text (? whitespace)))

(defrule maybe-whitespace-char ()
  (text (? whitespace-char)))

(defrule empty-line ()
  (progn #\newline (literal-string "")))

(defrule nonewline-line ()
  (postimes (pred #'not-newline character)))

(defrule a-char-line ()
  (postimes #\a))

(defrule maybe-newline ()
  (? #\newline))

(defrule non-empty-line ()
  (text (prog1 (postimes (pred #'not-newline character))
          (? #\newline))))

(defrule space ()
  #\space)

(defrule line ()
  (|| empty-line non-empty-line))

(defrule trimmed-line ()
  (string-trim '((literal-char #\space)
                 (literal-char #\tab))
               line))

(defrule trimmed-lines ()
  (times trimmed-line))

(defrule digits ()
  (text (postimes (pred #'digit-char-p character))))

(defrule integer ()
  (parse-integer (progm (? whitespace)
                        digits
                        (list (? whitespace) (|| (& #\,) (! character))))))

(defrule list-of-integers ()
  (let ((it (|| (list integer
		      #\,
		      (progn (format t (literal-string "I'm here!~%"))
			     (esrap-liquid::print-iter-state)
			     list-of-integers))
                integer)))
    (if (integerp it)
        (list it)
        (destructuring-bind (int comma list) it
          (declare (ignore comma))
          (cons int list)))))

(defrule single-token/bounds.1 ()
  (format nil (literal-string "~A[~S-~S]")
          (text (postimes (pred #'not-space character)))
          match-start
          match-end))

(defrule single-token/bounds.2 ()
  (format nil (literal-string "~C~A(~S-~S)")
          (pred #'not-space character)
          (text (times (pred #'not-space character)))
          match-start
          match-end))

(defrule tokens/bounds.1 ()
  (let ((match (progn (? whitespace)
                      (|| (cons single-token/bounds.1
                                (progn whitespace tokens/bounds.1))
                          single-token/bounds.1))))
    (if (stringp match)
        (list match)
        match)))

(defrule tokens/bounds.2 ()
  (let ((match (progn (? whitespace)
                      (|| (cons single-token/bounds.2
                                (progn whitespace tokens/bounds.2))
                          single-token/bounds.2))))
    (if (stringp match)
        (list match)
        match)))

(defrule left-recursion ()
  (progn left-recursion "l"))

(declaim (special *depth*))
(defvar *depth* nil)

(defrule around/inner ()
  (text (postimes (pred #'alpha-char-p character))))

(defrule around.1 ()
  (let ((*depth* (if *depth*
                     (cons (1+ (first *depth*)) *depth*)
                     (list 0))))
    (let ((it (|| around/inner
                  (list #\{ around.1 #\}))))
      (if (stringp it)
          (cons *depth* it)
          (second it)))))

(defrule around.2 ()
  (let ((it (|| around/inner
                (progm #\{ around.2 #\}))))
    (if (stringp it)
        `(((0 . (,match-start . ,match-end))) . ,it)
        `(((,(1+ (caaar it)) . (,match-start . ,match-end)) ,. (car it)) . ,(cdr it)))))

(defrule character-range ()
  (character-ranges (#\a #\b) #\-))

;; ;; Testing ambiguity when repetitioning possibly empty-string-match

(defrule spaces ()
  (length (times #\space)))

(defrule three-spaces ()
  (length (times #\space :exactly 3)))

(defrule upto-three-spaces ()
  (length (times #\space :upto 3)))

(defrule greedy-pos-spaces ()
  (postimes spaces))
(defrule greedy-spaces ()
  (times spaces))

;; Subtle bug here was caused by the fact, that SEPARATOR variable name
;; was the same as SEPARATOR rule-name.
(defparameter separator-char #\space)

(defrule simple-prefix ()
  (character-ranges (#\a #\z)))

(defun separator-p (x)
  (and (characterp x) (char= x separator-char)))

(defrule separator ()
  (pred #'separator-p character))

(defrule not-separator ()
  (!! separator))

(defrule word ()
  (text (postimes (!! separator))))

(defrule simple-wrapped ()
  (let ((separator-char simple-prefix))
    (prog1 (cons word
                 (times (progn separator word)))
      (? separator))))

(defparameter dyna-from-times 3)
(defparameter dyna-to-times 5)

(defrule dyna-from-to ()
  (text (times "a" :from dyna-from-times :upto dyna-to-times)))

(defrule dyna-from-tos ()
  (times dyna-from-to))

(defparameter context :void)

(defun in-context-p (x)
  (declare (ignore x))
  (and context (not (eql context :void))))

(defrule context-sensitive ()
  (pred #'in-context-p ""))
(defrule ooc-word ()
  word
  (literal-string "out of context word"))

(defrule cond-word ()
  dyna-from-to
  word)

(defrule foo+ ()
  (postimes "foo"))

(defrule bar+ ()
  (postimes "bar"))

(defrule decimal ()
  (parse-integer (format nil (literal-string "~{~A~}")
                         (postimes (|| "0" "1" "2" "3" "4" "5" "6" "7" "8" "9")))))

;;; Here we test correctness of defininion of parsing environments

(define-foo-rule abracadabra ()
  (literal-string "foo"))

(define-bar-rule abracadabra ()
  (literal-string "bar"))

(let ((map '((#\a . :a) (#\b . :b) (#\c . :c))))
  (defrule closure-rule ()
    (cdr (assoc (character-ranges (#\a #\c))
		map))))

(defrule dressed-elegantly ()
  "bar" "bar" "bar" c!-1-foo+ "bar" "bar" "bar"
  c!-1)

(defrule dressed-elegantly-2 ()
  (|| (progn "bar" "bar" "bar" c!-1-foo+ "bar" "bar" "bar")
      (progn "bar" "bar" c!-1-foo+ "bar" "bar"))
  c!-1)

(defrule cap-overwrite ()
  c!-1-bar+ c!-2-foo+ c!-2-bar+
  (list c!-1 c!-2))

(defrule f-opt-times (&optional (n 3))
  (times "f" :exactly n))


(defrule cipher ()
  (character-ranges (#\0 #\9)))

(defrule recurcapturing ()
  #\( (|| (progn c!-int-cipher c!-rc-recurcapturing)
	  #\a)
  #\)
  (cons c!-int c!-rc))



