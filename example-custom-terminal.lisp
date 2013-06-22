(defpackage :esrap-custom-terminal-example
  (:use :cl :esrap))

(in-package :esrap-custom-terminal-example)

;;; Ex. 1. Using a custom terminal for context sensitive parsing.

(defrule indent (+ #\space)
  (:lambda (spaces)
    (length spaces)))

(defrule line (and (+ (not #\newline)) #\newline)
  (:destructure (chars eol)
    (declare (ignore eol))
    (text chars)))

(defvar *current-indent*)

(defun current-indent-p (indent)
  (= indent *current-indent*))

(defrule block-at-current-indent (+ (and (current-indent-p indent) line))
  (:lambda (lines)
    (list :indent *current-indent* :lines (mapcar #'second lines))))

(defun indented-block-terminal (text position end)
  (let ((*current-indent* (parse 'indent text :start position :end end :junk-allowed t)))
    (parse 'block-at-current-indent-or-more text
           :start position :end end :junk-allowed t)))

(defrule indented-block #'indented-block-terminal)

(parse 'indented-block
"   foo
    bar
  quux"
       :junk-allowed t)

;;; Ex. 2. Using CL:READ to parse lisp.

(defun cl-read-terminal (text position end)
  (handler-case
      (read-from-string text t nil :start position :end end)
    (error ()
      (return-from cl-read-terminal (values nil 0)))))

(defrule common-lisp #'cl-read-terminal)

(eval (parse 'common-lisp "(list 'i 'love 'lisp)"))
