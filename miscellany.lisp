;;; miscellany.lisp

(in-package :esrap)

(defun text (&rest arguments)
  "Arguments must be strings, or lists whose leaves are strings.
Catenates all the strings in arguments into a single string."
  (with-output-to-string (s)
    (labels ((cat-list (list)
               (dolist (elt list)
                 (etypecase elt
                   (string (write-string elt s))
                   (character (write-char elt s))
                   (list (cat-list elt))))))
      (cat-list arguments))))

(setf (symbol-function 'concat) (symbol-function 'text))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun note-deprecated (old new)
    (warn 'simple-style-warning
          :format-control "~S is deprecated, use ~S instead."
          :format-arguments (list old new))))

(define-compiler-macro concat (&whole form &rest arguments)
  (declare (ignore arguments))
  (note-deprecated 'concat 'text)
  form)

(defun text/bounds (strings start end)
  (declare (ignore start end))
  (text strings))

(defun lambda/bounds (function)
  (lambda (result start end)
    (declare (ignore start end))
    (funcall function result)))

(defun identity/bounds (identity start end)
  (declare (ignore start end))
  identity)

(defun parse-lambda-list-maybe-containing-&bounds (lambda-list)
  "Parse &BOUNDS section in LAMBDA-LIST and return three values:

1. The standard lambda list sublist of LAMBDA-LIST
2. A symbol that should be bound to the start of a matching substring
3. A symbol that should be bound to the end of a matching substring
4. A list containing symbols that were GENSYM'ed.

The second and/or third values are GENSYMS if LAMBDA-LIST contains a
partial or no &BOUNDS section, in which case fourth value contains them
for use with IGNORE."
  (let ((length (length lambda-list)))
    (multiple-value-bind (lambda-list start end gensyms)
        (cond
          ;; Look for &BOUNDS START END.
          ((and (>= length 3)
                (eq (nth (- length 3) lambda-list) '&bounds))
           (values (subseq lambda-list 0 (- length 3))
                   (nth (- length 2) lambda-list)
                   (nth (- length 1) lambda-list)
                   nil))
          ;; Look for &BOUNDS START.
          ((and (>= length 2)
                (eq (nth (- length 2) lambda-list) '&bounds))
           (let ((end (gensym "END")))
             (values (subseq lambda-list 0 (- length 2))
                     (nth (- length 1) lambda-list)
                     end
                     (list end))))
          ;; No &BOUNDS section.
          (t
           (let ((start (gensym "START"))
                 (end (gensym "END")))
             (values lambda-list
                     start
                     end
                     (list start end)))))
      (check-type start symbol)
      (check-type end symbol)
      (values lambda-list start end gensyms))))

(deftype nonterminal ()
  "Any symbol except CHARACTER and NIL can be used as a nonterminal symbol."
  '(and symbol (not (member character nil))))

(deftype terminal ()
  "Literal strings and characters are used as case-sensitive terminal symbols,
and expressions of the form \(~ <literal>) denote case-insensitive terminals."
  `(or string character
       (cons (eql ~) (cons (or string character) null))))
