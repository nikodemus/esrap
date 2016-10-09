;;;; memoization.lisp

;;;; This is a part of esrap-liquid TDPL for Common Lisp
;;;; Alexander Popolitov, 2013
;;;; For licence, see COPYING

(in-package :esrap-liquid)

(defparameter contexts nil)
(defmacro register-context (context-sym)
  `(push ',context-sym contexts))

(defvar *cache*)

(defclass esrap-cache ()
  ((pos-hashtable :initform (make-hash-table :test #'equal))
   (start-pos :initform 0)))

(defun make-cache ()
  (make-instance 'esrap-cache))

(defun print-esrap-cache (cache)
  (format nil "[~a ~a]" (slot-value cache 'start-pos)
	  (mapcar (lambda (x)
		    (cons (car x) (hash->assoc (cdr x))))
		  (hash->assoc (slot-value cache 'pos-hashtable)))))

(defgeneric get-cached (symbol position args cache)
  (:documentation "Accessor for cached parsing results."))

(defun ensure-subpos-hash! (pos-hash pos)
  (multiple-value-bind (it got) (gethash pos pos-hash)
    (if got
	it
	(setf (gethash pos pos-hash) (make-hash-table :test #'equal)))))

(defmethod get-cached (symbol position args (cache esrap-cache))
  (with-slots (pos-hashtable) cache
    (let ((subpos-hash (ensure-subpos-hash! pos-hashtable position)))
      (if (context-sensitive-rule-p symbol)
	  (gethash `(,symbol ,args ,@(mapcar #'symbol-value contexts)) subpos-hash)
	  (gethash `(,symbol ,args) subpos-hash)))))

(defun (setf get-cached) (result symbol position args cache)
  (with-slots (pos-hashtable) cache
    (let ((subpos-hash (ensure-subpos-hash! pos-hashtable position)))
      (if (context-sensitive-rule-p symbol)
	  (setf (gethash `(,symbol ,args ,@(mapcar #'symbol-value contexts)) subpos-hash) result)
	  (setf (gethash `(,symbol ,args) subpos-hash) result)))))

(defmethod soft-shrink ((obj esrap-cache) num-elts-discarded)
  (with-slots (start-pos pos-hashtable) obj
    (iter (for i from start-pos to (1- num-elts-discarded))
	  (remhash i pos-hashtable))
    (incf start-pos num-elts-discarded)))

(defmethod hard-shrink ((obj esrap-cache) num-elts-discarded)
  (with-slots (start-pos pos-hashtable) obj
    (let ((new-hash (make-hash-table :test #'equal)))
      (iter (for (key val) in-hashtable pos-hashtable)
	    (if (>= key (+ start-pos num-elts-discarded))
		(setf (gethash (- key (+ start-pos num-elts-discarded)) new-hash)
		      val)))
      (setf start-pos 0
	    pos-hashtable new-hash))))


(defvar *nonterminal-stack* nil)

(defun hash->assoc (hash)
  (iter (for (key val) in-hashtable hash)
	(collect `(,key . ,val))))

(defun failed-parse-p (e)
  (typep e 'internal-esrap-error))

(defmacro with-cached-result ((symbol &rest args) &body forms)
  (with-gensyms (g!-args g!-position g!-result)
    `(let* ((,g!-args (list ,@args))
	    (,g!-position (+ the-position the-length))
	    (,g!-result (get-cached ',symbol ,g!-position ,g!-args *cache*))
	    (*nonterminal-stack* (cons ',symbol *nonterminal-stack*)))
       (cond ((eq :left-recursion ,g!-result)
	      (error 'left-recursion
		     :position ,g!-position
		     :nonterminal ',symbol
		     :path (reverse *nonterminal-stack*)))
	     (,g!-result (if-debug "~a (~{~s~^ ~}) ~a ~a: CACHED" ',symbol ,g!-args ,g!-position ,g!-result)
			 (print-iter-state the-iter)
			 (if (failed-parse-p ,g!-result)
			     (error ,g!-result)
			     (progn (incf the-length (cdr ,g!-result))
				    (fast-forward the-iter (cdr ,g!-result))
				    (car ,g!-result))))
	     (t
	      (if-debug "~a (~{~s~^ ~}) ~a ~a: NEW" ',symbol ,g!-args ,g!-position ,g!-result)
	      (print-iter-state the-iter)
	      ;; First mark this pair with :LEFT-RECURSION to detect left-recursion,
	      ;; then compute the result and cache that.
	      (setf (get-cached ',symbol ,g!-position ,g!-args *cache*) :left-recursion)
	      (multiple-value-bind (result length)
		  (handler-case (the-position-boundary
				 (values (progn ,@forms) the-length))
		    (internal-esrap-error (e) (values e :error)))
		;; (if-debug "after evaluation anew ~a ~a" length the-length)
		;; LENGTH is non-NIL only for successful parses
		(cond ((eq :error length) (setf (get-cached ',symbol ,g!-position ,g!-args *cache*)
						result)
		       (error result))
		      ((null length) (error "For some reason, length is NIL in memoization"))
		      (t (setf (get-cached ',symbol ,g!-position ,g!-args *cache*)
			       (cons result length))
			 (incf the-length length)
			 (if-debug "after setting cache ~a" the-length)
			 result))))))))
