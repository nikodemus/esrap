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
  (let ((it (|| (list integer #\, list-of-integers)
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

;; (declaim (special *depth*))
;; (defvar *depth* nil)

;; (defrule around/inner
;;     (+ (alpha-char-p character))
;;   (:text t))

;; (defrule around.1
;;     (or around/inner
;;         (and #\{ around.1 #\}))
;;   (:lambda (thing)
;;     (if (stringp thing)
;;         (cons *depth* thing)
;;         (second thing)))
;;   (:around ()
;;     (let ((*depth* (if *depth*
;;                        (cons (1+ (first *depth*)) *depth*)
;;                        (list 0))))
;;       (call-transform))))

;; (defrule around.2
;;     (or around/inner
;;         (and #\{ around.2 #\}))
;;   (:lambda (thing)
;;     (if (stringp thing)
;;         (cons *depth* thing)
;;         (second thing)))
;;   (:around (&bounds start end)
;;     (let ((*depth* (if *depth*
;;                        (cons (cons (1+ (car (first *depth*))) (cons start end))
;;                              *depth*)
;;                        (list (cons 0 (cons start end))))))
;;       (call-transform))))

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
