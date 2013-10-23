;;;; memoization.lisp

;;; MEMOIZATION CACHE
;;;
;;; Because each [rule, position] tuple has an unambiguous
;;; result per source text, we can cache this result -- this is what
;;; makes packrat parsing O(N).
;;;
;;; For now we just use EQUAL hash-tables, but a specialized
;;; representation would probably pay off.

(in-package :esrap)

(defparameter contexts nil)
(defmacro register-context (context-sym)
  `(push ',context-sym contexts))

(defvar *cache*)

(defun make-cache ()
  (make-hash-table :test #'equal))

(defun get-cached (symbol position cache)
  (gethash `(,symbol ,position ,@(mapcar #'symbol-value contexts)) cache))

(defun (setf get-cached) (result symbol position cache)
  (setf (gethash `(,symbol ,position ,@(mapcar #'symbol-value contexts)) cache) result))

(defvar *nonterminal-stack* nil)

(defun hash->assoc (hash)
  (iter (for (key val) in-hashtable hash)
	(collect `(,key . ,val))))

;;; SYMBOL, POSITION, and CACHE must all be lexical variables!
(defmacro with-cached-result ((symbol position &optional (text nil)) &body forms)
  (with-gensyms (cache result)
    `(let* ((,cache *cache*)
            (,result (get-cached ,symbol ,position ,cache))
            (*nonterminal-stack* (cons ,symbol *nonterminal-stack*)))
       (cond ((eq t ,result)
              (error 'left-recursion
                     :text ,text
                     :position ,position
                     :nonterminal ,symbol
                     :path (reverse *nonterminal-stack*)))
             (,result
              ,result)
             (t
              ;; First mark this pair with T to detect left-recursion,
              ;; then compute the result and cache that.
              (setf (get-cached ,symbol ,position ,cache) t
                    (get-cached ,symbol ,position ,cache) (locally ,@forms)))))))
