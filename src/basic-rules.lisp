;;;; basic-rules.lisp

;;;; This is a part of esrap-liquid TDPL for Common Lisp
;;;; Alexander Popolitov, 2013
;;;; For licence, see COPYING

(in-package #:esrap-liquid)

(enable-read-macro-tokens)

(def-nocontext-rule eof ()
  (handler-case (next-iter the-iter)
    (stop-iteration () (make-result 'eof))
    (:no-error (token)
      (declare (ignore token))
      (rel-rewind the-iter)
      (fail-parse (literal-string "Not at the end of token stream.")))))

(def-nocontext-rule sof ()
  (if (start-of-iter-p the-iter)
      (make-result 'sof)
      (fail-parse (literal-string "Not at the start of token stream."))))

(def-nocontext-rule any-string (length)
  (let ((pre-res (handler-case (iter (for i from 1 to length)
				     (collect (next-iter the-iter)))
		   (stop-iteration ()
		     (fail-parse (literal-string "EOF while trying to parse any string of specified length."))))))
    (make-result (coerce pre-res 'string) length)))
        
(defmacro any-string (length)
  `(descend-with-rule 'any-string ,length))

(def-nocontext-rule any-token ()
  (make-result (handler-case (next-iter the-iter)
		 (stop-iteration ()
		   (fail-parse (literal-string "EOF reached while trying to parse any token."))))
	       1))
  

(def-nocontext-rule character (char)
  (let ((it (handler-case (next-iter the-iter)
	      (stop-iteration () (fail-parse (literal-string "EOF reached while trying to parse character."))))))
    (format t (literal-string "        in character: ~s ~s~%") it char)
    (print-iter-state the-iter)
    (if (not char)
	(make-result it 1)
	(if (char= it char)
	    (progn (format t (literal-string "         succeeding in character!~%"))
		   (make-result it 1))
	    (fail-parse-format (literal-string "Char ~a is not equal to desired char ~a") it char)))))

(def-nocontext-rule string (string)
  (let ((any-string (any-string (length string))))
    (if (string= any-string string)
        (make-result any-string)
        (fail-parse-format (literal-string "String ~a is not equal to desired string ~a")
			   any-string
			   string))))

(defun joinl (joinee lst)
  (format nil (strcat "~{~a~^" joinee "~}") lst))


