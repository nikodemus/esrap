;;;; basic-rules.lisp

;;;; This is a part of esrap-liquid TDPL for Common Lisp
;;;; Alexander Popolitov, 2013
;;;; For licence, see COPYING

(in-package #:esrap-liquid)

(enable-read-macro-tokens)

(defrule any-string (length)
  (let ((limit (+ length position)))
    (if (<= limit end)
        (make-result (subseq text position limit) length)
        (fail-parse (literal-string "Unable to parse any string of specified length.")))))
(defmacro any-string (length)
  `(descend-with-rule 'any-string ,length))


(defrule character (char)
  (if (< position end)
      (if char
          (let ((it (char text position)))
            (if (char= it char)
                (make-result it 1)
                (fail-parse-format (literal-string "Char ~a is not equal to desired char ~a") it char)))
          (make-result (char text position) 1))
      (fail-parse (literal-string "EOF reached while trying to parse character."))))

(defrule string (string)
  (let ((any-string (any-string (length string))))
    (if (string= any-string string)
        (make-result any-string)
        (fail-parse-format (literal-string "String ~a is not equal to desired string ~a")
			   any-string
			   string))))

(defun joinl (joinee lst)
  (format nil (strcat "~{~a~^" joinee "~}") lst))


