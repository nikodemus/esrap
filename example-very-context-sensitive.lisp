;;;; Esrap example: grammar, in which context is determined by a natural number

(require :esrap)

(defpackage :very-context-sensitive
  (:use :cl :esrap))

(in-package :very-context-sensitive)

(defparameter indent 0 "Indent that is stripped from all lines.")

(defrule spaces (* #\space)
  (:lambda (lst)
    (length lst)))

(defun indented-p (len)
  (>= len indent))

(defrule indented-spaces (indented-p spaces)
  (:lambda (len)
    (- len indent)))
  
(defrule digit (character-ranges (#\0 #\9)))

(defrule indent-spec-line (and spaces "|" (+ digit) "|" spaces #\newline)
  (:destructure (wh0 ch0 digits ch1 wh1 nl0)
		(declare (ignore wh0 ch0 ch1 wh1 nl0))
		(parse-integer (text digits))))

(defrule indented-line (and indented-spaces (* (not #\newline)) #\newline)
  (:destructure (isps line nl0)
		(declare (ignore nl0))
		(text (make-string isps :initial-element #\space)
		      line)))

(defrule explicit-indented-block (wrap indent-spec-line
				       (* (and (! indent-spec-line)
					       indented-line)))
  (:wrap-around (let ((indent wrapper))
		  (call-parser)))
  (:lambda (lst)
    (mapcar #'cadr lst)))

(defrule explicit-blocks (+ explicit-indented-block))

(defrule implicit-indented-block (wrap ""
				       (* (and (! indent-spec-line)
					       indented-line)))
  (:wrap-around (let ((indent 0))
		  (call-parser)))
  (:lambda (lst)
    (mapcar #'cadr lst)))

(defrule implicit-blocks (+ implicit-indented-block))

(defrule indented-block (or explicit-indented-block
			    implicit-indented-block))

(defrule blocks (+ indented-block))

(defrule multi-spaces (+ spaces))
   
		
