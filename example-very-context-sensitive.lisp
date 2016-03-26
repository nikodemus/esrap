;;;; Esrap example: grammar, in which context is determined by a natural number

(require :esrap-liquid)

(defpackage :very-context-sensitive
  (:use :cl :esrap-liquid))

(in-package :very-context-sensitive)

(enable-read-macro-tokens)

(defparameter indent 0 "Indent that is stripped from all lines.")

(defrule spaces ()
  (length (times #\space)))

(defun indented-p (len)
  (>= len indent))

(defrule indented-spaces ()
  (- (pred #'indented-p spaces)
     indent))
  
(defrule digit ()
  (character-ranges (#\0 #\9)))

(defrule indent-spec-line ()
  (parse-integer (text (progm (progn (v spaces) (v "|"))
                              (postimes digit)
                              (progn (v "|") (v spaces) (v #\newline))))))

(defrule indented-line ()
  (prog1 (text (make-string (v indented-spaces) :initial-element #\space)
               (times (!! #\newline)))
    (v #\newline)))

(defun more-indented-block-p (explicit-block)
  (>= (caddr explicit-block)
      indent))

(defrule explicit-indented-block ()
  (let ((indent indent-spec-line))
    `(expl-block :indent ,indent
                 :contents ,(times (|| (pred #'more-indented-block-p
                                             explicit-indented-block)
                                       (progn (! indent-spec-line)
                                              indented-line))))))

(defrule explicit-blocks ()
  (postimes explicit-indented-block))

;; (defrule implicit-indented-block (wrap ""
;; 				       (* (and (! indent-spec-line)
;; 					       indented-line)))
;;   (:wrap-around (let ((indent 0))
;; 		  (call-parser)))
;;   (:lambda (lst)
;;     (mapcar #'cadr lst)))

;; (defrule implicit-blocks (+ implicit-indented-block))

;; (defrule indented-block (or implicit-indented-block
;; 			    explicit-indented-block))

;; (defrule blocks (* indented-block))

;; (defrule multi-spaces (+ spaces))
   
		
