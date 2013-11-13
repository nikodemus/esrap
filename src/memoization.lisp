;;;; memoization.lisp

;;;; This is a part of esrap-liquid TDPL for Common Lisp
;;;; Alexander Popolitov, 2013
;;;; For licence, see COPYING

(in-package :esrap-liquid)

(defparameter contexts nil)
(defmacro register-context (context-sym)
  `(push ',context-sym contexts))

(defvar *cache*)

(defun make-cache ()
  (make-hash-table :test #'equal))

(defun get-cached (symbol position args cache)
  (gethash `(,symbol ,position ,args ,@(mapcar #'symbol-value contexts)) cache))

(defun (setf get-cached) (result symbol position args cache)
  (setf (gethash `(,symbol ,position ,args ,@(mapcar #'symbol-value contexts)) cache) result))

(defvar *nonterminal-stack* nil)

(defun hash->assoc (hash)
  (iter (for (key val) in-hashtable hash)
	(collect `(,key . ,val))))

(defun failed-parse-p (e)
  (typep e 'simple-esrap-error))

;;; SYMBOL, POSITION, and CACHE must all be lexical variables!
(defmacro! with-cached-result ((symbol position text &rest args) &body forms)
  `(let* ((,g!-cache *cache*)
          (,g!-args (list ,@args))
          (,g!-position ,position)
          (,g!-result (get-cached ',symbol ,g!-position ,g!-args ,g!-cache))
          (*nonterminal-stack* (cons ',symbol *nonterminal-stack*)))
     ;; (format t "hashassoc ~a~%" (hash->assoc ,g!-cache))
     ;; (format t "sym: ~a pos: ~a res: ~a~%" ',symbol ,g!-position ,g!-result)
     (cond ((eq :left-recursion ,g!-result)
            (error 'left-recursion
                   :text ,text
                   :position ,g!-position
                   :nonterminal ',symbol
                   :path (reverse *nonterminal-stack*)))
           (,g!-result (if (failed-parse-p ,g!-result)
                           (error ,g!-result)
                           (values (car ,g!-result) (cdr ,g!-result))))
           (t
            ;; First mark this pair with :LEFT-RECURSION to detect left-recursion,
            ;; then compute the result and cache that.
            (setf (get-cached ',symbol ,g!-position ,g!-args ,g!-cache) :left-recursion)
            ;; (format t "hashassoc 2 ~a~%" (hash->assoc ,g!-cache))
            (multiple-value-bind (result position) (handler-case (locally ,@forms)
                                                     (simple-esrap-error (e) e))
              ;; POSITION is non-NIL only for successful parses
              (if position
                  (progn (setf (get-cached ',symbol ,g!-position ,g!-args ,g!-cache)
                               (cons result position))
                         ;; (format t "hashassoc 2.5 ~a~%" (hash->assoc ,g!-cache))
                         (values result position))
                  (progn (setf (get-cached ',symbol ,g!-position ,g!-args ,g!-cache)
                               result)
                         ;; (format t "hashassoc 3 ~a~%" (hash->assoc ,g!-cache))
                         (error result))))))))
