;;;; esrap-env.lisp

;;;; This is a part of esrap-liquid TDPL for Common Lisp
;;;; Alexander Popolitov, 2013
;;;; For licence, see COPYING

(in-package :esrap-liquid)

(defmacro! in-esrap-env (symbol)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defmacro!! ,e!-define-rule (symbol args &body body)
         ()
       `(,',(if symbol
                (symbolicate "DEFINE-" symbol "-RULE")
                'defrule)
            ,symbol ,args ,@body))))

(defun install-common-rules (hash-table)
  (let ((common-rules '(any-string character string)))
    (iter (for rule in common-rules)
	  (setf (gethash rule hash-table)
		(gethash rule *rules*)))))

(defmacro define-esrap-env (symbol)
  `(progn (eval-when (:compile-toplevel :load-toplevel :execute)
	    (defvar ,(symbolicate symbol "-RULES") (make-hash-table))
	    (install-common-rules ,(symbolicate symbol "-RULES"))
	    (defvar ,(symbolicate symbol "-CONTEXTS") nil))
	  (defmacro ,(symbolicate "WITH-" symbol "-RULES") (&body body)
	    `(let ((esrap-liquid::*rules* ,',(symbolicate symbol "-RULES")))
	       ,@body))
	  (defmacro ,(symbolicate "WITH-" symbol "-CONTEXTS") (&body body)
	    `(let ((esrap-liquid::contexts ,',(symbolicate symbol "-CONTEXTS")))
	       ,@body))
	  (defmacro!! ,(symbolicate "DEFINE-" symbol "-RULE") (symbol args &body body)
              ()
	    `(,',(symbolicate "WITH-" symbol "-RULES")
		 (,',(symbolicate "WITH-" symbol "-CONTEXTS")
		     (defrule ,symbol ,args ,@body))))
	  (defmacro ,(symbolicate "REGISTER-" symbol "-CONTEXT")
	      (context-var &rest plausible-contexts)
	    `(progn (defparameter ,context-var ,(make-keyword (format nil "~a" (car plausible-contexts))))
		    ,@(mapcar (lambda (context-name)
				(let ((pred-name (symbolicate context-name
								     "-"
								     context-var
								     "-P"))
				      (rule-name (symbolicate context-name
								     "-"
								     context-var)))
				  `(progn
				     (defun ,pred-name (x)
				       (declare (ignore x))
				       (equal ,context-var ,(make-keyword context-name)))
				     (,',(symbolicate "DEFINE-" symbol "-RULE") ,rule-name ()
				       ;; KLUDGE: probably, special reader syntax for defining rules
				       ;; will not work here anyway
				       (pred #',pred-name t)
				       nil))))
			      (mapcar (lambda (x) (format nil "~a" x)) plausible-contexts))
		    (push ',context-var ,',(symbolicate symbol "-CONTEXTS"))))
	  (defmacro!! ,(symbolicate symbol "-PARSE")
	      (expression text &key (start nil start-p)
                          (end nil end-p)
                          (junk-allowed nil junk-allowed-p))
              ()
	    `(,',(symbolicate "WITH-" symbol "-RULES")
		 (,',(symbolicate "WITH-" symbol "-CONTEXTS")
		     (parse ,(if (and (consp expression)
				      (eql (car expression) 'quote)
				      (equal (length expression) 2)
				      (symbolp (cadr expression))
				      (not (keywordp (cadr expression))))
				 `',(intern (string (cadr expression))
					    ',*package*)
				 expression)
			    ,text
			    ,@(if start-p `(:start ,start))
			    ,@(if end-p `(:end ,end))
			    ,@(if junk-allowed-p
				  `(:junk-allowed ,junk-allowed))))))))

			

;; This is the example of macroexpansion
#+nil
(define-esrap-env yaclyaml)
