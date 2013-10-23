;;;; rule-storage.lisp

(in-package :esrap)


;;; RULE REPRESENTATION AND STORAGE
;;;
;;; For each rule, there is a RULE-CELL in *RULES*, whose %INFO slot has the
;;; function that implements the rule in car, and the rule object in CDR. A
;;; RULE object can be attached to only one non-terminal at a time, which is
;;; accessible via RULE-SYMBOL.

(defvar *rules* (make-hash-table))

(defun clear-rules ()
  (clrhash *rules*)
  nil)

(defstruct (rule-cell (:constructor
                       make-rule-cell
                       (symbol &aux (%info (cons (undefined-rule-function symbol) nil))))
                      (:conc-name cell-))
  (%info (required-argument) :type (cons function t))
  (trace-info nil)
  (referents nil :type list))

(declaim (inline cell-function))
(defun cell-function (cell)
  (car (cell-%info cell)))

(defun cell-rule (cell)
  (cdr (cell-%info cell)))

(defun set-cell-info (cell function rule)
  ;; Atomic update
  (setf (cell-%info cell) (cons function rule))
  (let ())
  cell)

(defun undefined-rule-function (symbol)
  (lambda (&rest args)
    (declare (ignore args))
    (error "Undefined rule: ~S" symbol)))

(defun ensure-rule-cell (symbol)
  (check-type symbol nonterminal)
  ;; FIXME: Need to lock *RULES*.
  (or (gethash symbol *rules*)
      (setf (gethash symbol *rules*)
            (make-rule-cell symbol))))

(defun delete-rule-cell (symbol)
  (remhash symbol *rules*))

(defun reference-rule-cell (symbol referent)
  (let ((cell (ensure-rule-cell symbol)))
    (when referent
      (pushnew referent (cell-referents cell)))
    cell))

(defun dereference-rule-cell (symbol referent)
  (let ((cell (ensure-rule-cell symbol)))
    (setf (cell-referents cell) (delete referent (cell-referents cell)))
    cell))

(defun find-rule-cell (symbol)
  (check-type symbol nonterminal)
  (gethash symbol *rules*))

(defclass rule ()
  ((%symbol
    :initform nil)
   (%expression
    :initarg :expression
    :initform (required-argument :expression))
   (%guard-expression
    :initarg :guard-expression
    :initform t
    :reader rule-guard-expression)
   ;; Either T for rules that are always active (the common case),
   ;; NIL for rules that are never active, or a function to call
   ;; to find out if the rule is active or not.
   (%condition
    :initarg :condition
    :initform t
    :reader rule-condition)
   (%transform
    :initarg :transform
    :initform nil
    :reader rule-transform)
   (%around
    :initarg :around
    :initform nil
    :reader rule-around)))

(defun rule-symbol (rule)
  "Returns the nonterminal associated with the RULE, or NIL of the rule
is not attached to any nonterminal."
  (slot-value rule '%symbol))

(defun detach-rule (rule)
  (dolist (dep (%rule-direct-dependencies rule))
    (dereference-rule-cell dep (rule-symbol rule)))
  (setf (slot-value rule '%symbol) nil))

(defmethod shared-initialize :after ((rule rule) slots &key)
  (validate-expression (rule-expression rule)))

(defmethod print-object ((rule rule) stream)
  (print-unreadable-object (rule stream :type t :identity nil)
    (let ((symbol (rule-symbol rule)))
      (if symbol
          (format stream "~S <- " symbol)
          (format stream "(detached) ")))
    (write (rule-expression rule) :stream stream)))

(defun sort-dependencies (symbol dependencies)
  (let ((symbols (delete symbol dependencies))
        (defined nil)
        (undefined nil))
    (dolist (sym symbols)
      (if (find-rule sym)
          (push sym defined)
          (push sym undefined)))
    (values defined undefined)))

(defun rule-dependencies (rule)
  "Returns the dependencies of the RULE: primary value is a list of defined
nonterminal symbols, and secondary value is a list of undefined nonterminal
symbols."
  (sort-dependencies
   (rule-symbol rule) (%expression-dependencies (rule-expression rule) nil)))

(defun rule-direct-dependencies (rule)
  (sort-dependencies
   (rule-symbol rule) (%expression-direct-dependencies (rule-expression rule) nil)))

(defun %rule-direct-dependencies (rule)
  (delete (rule-symbol rule) (%expression-direct-dependencies (rule-expression rule) nil)))

