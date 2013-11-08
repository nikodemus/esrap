;;;; memoization.lisp

;;;; This is a part of esrap-liquid TDPL for Common Lisp
;;;; Alexander Popolitov, 2013
;;;; For licence, see COPYING

(in-package :esrap)

(defparameter contexts nil)
(defmacro register-context (context-sym)
  `(push ',context-sym contexts))

(defvar *cache*)

(defun make-cache ()
  (make-hash-table :test #'equal))

(defun get-cached (symbol position args cache)
  (gethash `(,symbol ,position ,args ,@(mapcar #'symbol-value contexts)) cache))

(defun (setf get-cached) (result symbol position cache)
  (setf (gethash `(,symbol ,position ,@(mapcar #'symbol-value contexts)) cache) result))

(defvar *nonterminal-stack* nil)

(defun hash->assoc (hash)
  (iter (for (key val) in-hashtable hash)
	(collect `(,key . ,val))))

(defun failed-parse-p (e)
  (typep e 'simple-esrap-error))

;;; SYMBOL, POSITION, and CACHE must all be lexical variables!
(defmacro! with-cached-result ((symbol position text &rest args) &body forms)
  `(let* ((,g!-cache *cache*)
          (,g!-result (get-cached ',symbol ,position (list ,@args) ,g!-cache))
          (*nonterminal-stack* (cons ',symbol *nonterminal-stack*)))
     (cond ((eq :left-recursion ,g!-result)
            (error 'left-recursion
                   :text ,text
                   :position ,position
                   :nonterminal ',symbol
                   :path (reverse *nonterminal-stack*)))
           (,g!-result (if (failed-parse-p ,g!-result)
                           (error ,g!-result)
                           (values (car ,g!-result) (cdr ,g!-result))))
           (t
            ;; First mark this pair with :LEFT-RECURSION to detect left-recursion,
            ;; then compute the result and cache that.
            (setf (get-cached ',symbol ,position ,g!-cache) :left-recursion)
            (multiple-value-bind (result position) (handler-case (locally ,@forms)
                                                     (simple-esrap-error (e) e))
              ;; POSITION is non-NIL only for successful parses
              (if position
                  (progn (setf (get-cached ',symbol ,position ,g!-cache)
                               (cons result position))
                         (values result position))
                  (progn (setf (get-cached ',symbol ,position ,g!-cache)
                               result)
                         (error result))))))))
