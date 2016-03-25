;;;; macro.lisp

;;;; This is a part of esrap-liquid TDPL for Common Lisp
;;;; Alexander Popolitov, 2013
;;;; For licence, see COPYING

(in-package #:esrap-liquid)

(cl-interpol:enable-interpol-syntax)

(defmacro! descend-with-rule (o!-sym &rest args)
  `(multiple-value-bind (,g!-it ,g!-got) (gethash ,o!-sym *rules*)
     (if (not ,g!-got)
         (error "Undefined rule: ~s" ,o!-sym)
	 (tracing-level
	   (funcall ,g!-it ,@args)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro-enhance::def-*!-symbol-p c)
  (defun parse-c!-symbol (sym)
    (cl-ppcre:register-groups-bind (second third)
	("^C!-([^-]+)(.*)" (string sym))
      (values (intern (concatenate 'string "C!-" second))
	      (if (string= "" third)
		  nil
		  (subseq third 1))))))
      

(defmacro the-position-boundary (&body body)
  `(let* ((the-position (+ the-position the-length))
	  (the-length 0))
     ,@body))

(defun wrap-with-esrap-macrolets (body)
  `(macrolet ((v (thing &rest args)
		(cond ((characterp thing) (if args
					      (error "Descent with character has extra argument, &
                                                                  but it shouldn't")
					      `(descend-with-rule 'character ,thing)))
		      ((stringp thing) (if args
					   (error "Descent with string has extra argument, &
                                                               but it shouldn't")
					   `(descend-with-rule 'string ,thing)))
		      ((symbolp thing) `(descend-with-rule ',thing ,@args))
		      (t (error "Don't know how to descend with this : ~a" thing)))))
     ,body))


(defun! make-rule-lambda (name args body)
  (multiple-value-bind (reqs opts rest kwds allow-other-keys auxs kwds-p) (parse-ordinary-lambda-list args)
    (declare (ignore kwds))
    (if kwds-p (error "&KEY arguments are not supported"))
    (if allow-other-keys (error "&ALLOW-OTHER-KEYS is not supported"))
    (if auxs (error "&AUX variables are not supported, use LET"))
    (wrap-with-esrap-macrolets
     `(named-lambda ,(intern (strcat "ESRAP-" name)) (,@args)
	(with-cached-result (,name ,@reqs
				   ,@(if rest
					 `(,rest)
					 (iter (for (opt-name opt-default opt-supplied-p) in opts)
					       (collect opt-name)
					       (if opt-supplied-p
						   (collect opt-supplied-p)))))
	  ,@body)))))



(defmacro %defrule (name args &body body)
  (if-debug-fun "I'm starting to actually expand ~a!" name)
  ;; TODO: bug - C!-vars values are kept between different execution of a rule!
  `(setf (gethash ',name *rules*)
	 ,(make-rule-lambda name args body)))

(defmacro defrule (name args &body body)
  `(progn (%defrule ,name ,args ,@body)
	  (setf (gethash ',name *rule-context-sensitivity*) t)))

(defmacro def-nocontext-rule (name args &body body)
  `(progn (%defrule ,name ,args ,@body)
	  (setf (gethash ',name *rule-context-sensitivity*) nil)))


(defmacro! make-result (result &optional (length 0) beginning)
  ;; We must preserve the semantics, that computation of results occurs before increment of length
  `(let ((,g!-result ,result))
     (incf the-length ,length)
     (if-debug "~asuccess: ~s ~a" ,(if beginning
				       #?"$(beginning) "
				       "")
	       ,g!-result the-length)
     ,g!-result))


(defmacro! || (&rest clauses)
  `(tracing-level
     (if-debug "||")
     (multiple-value-bind (,g!-result ,g!-the-length)
	 ;; All this tricky business with BLOCK just for automatic LENGTH tracking.
	 (block ,g!-ordered-choice
	   (let (,g!-parse-errors)
	     ,@(mapcar (lambda (clause)
			 `(the-position-boundary
			    (print-iter-state)
			    (with-saved-iter-state (the-iter)
			      (handler-case (return-from ,g!-ordered-choice
					      (let ((res ,clause))
						;; (if-debug "|| pre-succeeding")
						(values res the-length)))
				(simple-esrap-error (e)
				  (restore-iter-state)
				  (push e ,g!-parse-errors))))))
		       clauses)
	     (if-debug "|| before failing P ~a L ~a" the-position the-length)
	     (fail-parse (joinl "~%"
				(mapcar (lambda (x)
					  (slot-value x 'reason))
					(nreverse ,g!-parse-errors))))))
       (if-debug "|| aftermath ~a ~a" the-length ,g!-the-length)
       (incf the-length ,g!-the-length)
       ,g!-result)))

(defmacro! most-full-parse (&rest clauses)
  `(tracing-level
     (if-debug "MOST-FULL-PARSE")
     (multiple-value-bind (,g!-result ,g!-the-length)
	 ;; All this tricky business with BLOCK just for automatic LENGTH tracking.
	 (block ,g!-most-full-parse
	   (let (,g!-parse-errors ,g!-successful-parses)
	     ,@(mapcar (lambda (clause)
			 `(the-position-boundary
			    (print-iter-state)
			    (with-saved-iter-state (the-iter)
			      (handler-case ,clause
				(simple-esrap-error (e)
				  (restore-iter-state)
				  (push e ,g!-parse-errors))
				(:no-error (res)
				  (restore-iter-state)
				  (push (list res the-length) ,g!-successful-parses))))))
		       clauses)
	     (if ,g!-successful-parses
		 (destructuring-bind (res length) (car (sort ,g!-successful-parses #'> :key #'cadr))
		   (fast-forward the-iter length)
		   (values res length))
		 (progn (if-debug "|| before failing P ~a L ~a" the-position the-length)
			(fail-parse (joinl "~%"
					   (mapcar (lambda (x)
						     (slot-value x 'reason))
						   (nreverse ,g!-parse-errors))))))))
       (if-debug "MOST-FULL-PARSE aftermath ~a ~a" the-length ,g!-the-length)
       (incf the-length ,g!-the-length)
       ,g!-result)))


(defmacro ! (expr)
  "Succeeds, whenever parsing of EXPR fails. Does not consume, returns NIL, for compatibility with TEXT"
  `(tracing-level
     (if-debug "! P ~a L ~a" the-position the-length)
     (the-position-boundary
       (with-saved-iter-state (the-iter)
	 (handler-case ,expr
	   (simple-esrap-error ()
	     (restore-iter-state)
	     nil)
	   (:no-error (result)
	     (declare (ignore result))
	     (fail-parse "Clause under non-consuming negation succeeded.")))))
     (if-debug "! before result P ~a L ~a" the-position the-length)
     (make-result nil 0)))

(defmacro !! (expr)
  "Succeeds, whenever parsing of EXPR fails. Consumes, assumes than EXPR parses just one character."
  `(tracing-level
     (if-debug "!!")
     (the-position-boundary
       (with-saved-iter-state (the-iter)
	 (handler-case ,expr
	   (simple-esrap-error ()
	     (restore-iter-state)
	     nil)
	   (:no-error (result)
	     (declare (ignore result))
	     (fail-parse "Clause under consuming negation succeeded.")))))
     (descend-with-rule 'any-token)))
              

(defmacro! times (subexpr &key from upto exactly)
  (flet ((frob (condition)
           `(let (,g!-result)
	      (tracing-level
		(iter ,(if (or upto exactly)
			   `(for ,g!-i from 1 to ,(or upto exactly)))
		      (if-debug "TIMES")
		      ;; (print-iter-state the-iter)
		      (multiple-value-bind (,g!-subresult ,g!-the-length)
			  (with-saved-iter-state (the-iter)
			    ;; (format t "   Inside subexpression:~%")
			    (handler-case (the-position-boundary
					    (let ((subexpr ,subexpr))
					      ;; (format t "    succeeding ~s ~a~%" subexpr the-length)
					      ;; (print-iter-state the-iter)
					      (values subexpr the-length)))
			      (simple-esrap-error ()
				;; (format t "    failing~%")
				(restore-iter-state)
				(finish))))
			(if-first-time nil
				       (if (equal ,g!-the-length 0)
					   (terminate)))
			(push ,g!-subresult ,g!-result)
			(incf the-length ,g!-the-length))
		      (finally (if ,condition
				   (return (make-result (nreverse ,g!-result)))
				   (fail-parse "Greedy repetition failed."))))))))
    (cond (exactly (if (or from upto)
                       (error "keywords :EXACTLY and :FROM/:UPTO are mutually exclusive.")
                       (frob `(equal (length ,g!-result) ,exactly))))
          (from (if upto
                    (frob `(and (>= (length ,g!-result) ,from)
                                (<= (length ,g!-result) ,upto)))
                    (frob `(>= (length ,g!-result) ,from))))
          (upto (frob `(<= (length ,g!-result) ,upto)))
          (t (frob t)))))
  
(defmacro postimes (subexpr)
  `(times ,subexpr :from 1))

(defmacro! pred (predicate subexpr)
  `(tracing-level
     (if-debug "PREDICATE")
     (let ((,g!-it ,subexpr))
       (if (funcall ,predicate ,g!-it)
	   ,g!-it
	   (fail-parse "Predicate test failed")))))

(defmacro progm (start meat end)
  "Prog Middle."
  `(progn ,start (prog1 ,meat ,end)))

(defmacro! ? (subexpr)
  `(tracing-level
     (if-debug "? ~a ~a" the-position the-length)
     (multiple-value-bind (,g!-result ,g!-the-length)
	 (block ,g!-?
	   (the-position-boundary
	     (print-iter-state)
	     (with-saved-iter-state (the-iter)
	       (handler-case ,subexpr
		 (simple-esrap-error ()
		   (restore-iter-state)
		   (values nil nil))
		 (:no-error (result) (return-from ,g!-? (values result the-length)))))))
       (when ,g!-the-length
	 (incf the-length ,g!-the-length))
       ,g!-result)))

(defmacro & (subexpr)
  `(tracing-level
     (if-debug "&")
     (make-result (the-position-boundary
		    (with-saved-iter-state (the-iter)
		      (let ((it ,subexpr))
			(restore-iter-state)
			it))))))
			

(defmacro -> (subexpr)
  (tracing-level
    (if-debug "->")
    (if (and (symbolp subexpr) (equal (string subexpr) "EOF"))
	`(progn (descend-with-rule 'eof) nil)
	`(progn (the-position-boundary
		  (with-saved-iter-state (the-iter)
		    ,subexpr
		    (restore-iter-state)))
		(make-result nil)))))

(defmacro! <- (subexpr)
  `(tracing-level
     (if-debug "<-")
     ,(if (and (symbolp subexpr) (equal (string subexpr) "SOF"))
	  `(progn (descend-with-rule 'sof) nil)
	  `(progn (the-position-boundary
		    (handler-case (progn (rel-rewind the-iter)
					 (decf the-position))
		      (buffer-error ()
			(fail-parse "Can't rewind back even by 1 token")))
		    ,subexpr
		    (if (not (equal the-length 1))
			(fail-parse "Parsing of subexpr took more than 1 token.")))
		  nil))))
	     

(defmacro! cond-parse (&rest clauses)
  `(|| ,@(mapcar (lambda (clause)
                   `(progn ,@clause))
                 clauses)))


(defun crunch-c!-s (pre-body)
  (declare (special c!-vars))
  ;; (format t "pre-body ~a~%" pre-body)
  (destructuring-bind (labels ((name args . body))
			. body2) pre-body
    (declare (ignore labels))
    `(labels ((,name ,args
		(let ,(iter (for (key nil) in-hashtable c!-vars)
			    (collect key))
		  ,@body)))
       ,@body2)))
    
