;;;; rule-storage.lisp

;;;; This is a part of esrap-liquid TDPL for Common Lisp
;;;; Alexander Popolitov, 2013
;;;; For licence, see COPYING

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


