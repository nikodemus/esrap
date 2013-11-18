;;;; Esrap example: a simple grammar with scopes and symbol tables.

(require :esrap-liquid)

(defpackage :symbol-table
  (:use :cl :esrap-liquid))

(in-package :symbol-table)

(enable-read-macro-tokens)

(declaim (special *symbol-table*))
(defvar *symbol-table* nil)

(defstruct (symbol-table
            (:constructor make-symbol-table (&optional %parent)))
  (%table (make-hash-table :test #'equal))
  %parent)

(defun lookup/direct (name &optional (table *symbol-table*))
  (values (gethash name (symbol-table-%table table))))

(defun lookup (name &optional (table *symbol-table*))
  (or (lookup/direct name table)
      (alexandria:when-let ((parent (symbol-table-%parent table)))
        (lookup name parent))))

(defun (setf lookup) (new-value name &optional (table *symbol-table*))
  (when (lookup/direct name table)
    (error "~@<Duplicate name: ~S.~@:>"
           name))
  (setf (gethash name (symbol-table-%table table)) new-value))



(defrule whitespace ()
  (postimes (|| #\Space #\Tab #\Newline))
  nil)

(defrule name ()
  (text (postimes (pred #'alphanumericp character))))

(defrule type ()
  (times (postimes (pred #'alphanumericp character))))

(defrule declaration ()
  (destructuring-bind (name colon type) (list name #\: type)
    (declare (ignore colon))
    (setf (lookup name) (list name :type type))
    (values)))

(defrule use ()
  (let ((name name))
    (list :use (or (lookup name)
                   (error "~@<Undeclared variable: ~S.~@:>"
                          name)))))
    
(defrule statement ()
  (remove nil (postimes (|| scope declaration use))))

(defrule statement/ws ()
  (prog1 statement (? whitespace)))

(defrule scope ()
  (let ((*symbol-table* (make-symbol-table *symbol-table*)))
    (list* :scope (apply #'append 
                         (progm (progn #\{ (? whitespace))
                                (* statement/ws)
                                (progn #\} (? whitespace)))))))

(parse 'scope "{
  a:int
  a
  {
    a
    b:double
    a
    b
    {
      a:string
      a
      b
    }
    a
    b
  }
  a
}")
