;;;; macro.lisp

;;;; This is a part of esrap-liquid TDPL for Common Lisp
;;;; Alexander Popolitov, 2013
;;;; For licence, see COPYING

(in-package #:esrap-liquid)

(defmacro! descend-with-rule (o!-sym &rest args)
  `(multiple-value-bind (,g!-it ,g!-got) (gethash ,o!-sym *rules*)
     ;; (format t "sym: ~a ~a~%" ,o!-sym position)
     (if (not ,g!-got)
         (error "Undefined rule: ~s" ,o!-sym)
         (multiple-value-bind (result new-position) (funcall ,g!-it text position end ,@args)
           ;; (format t "position before setting ~a, after setting would be ~a" position new-position)
           (setf position new-position)
           result))))

(defmacro with-esrap-reader-context (&body body)
  `(let ((char-reader (get-dispatch-macro-character #\# #\\))
          (string-reader (get-macro-character #\")))
      (with-dispatch-macro-character (#\# #\\ (esrap-char-reader char-reader))
        (with-macro-character (#\" (esrap-string-reader string-reader))
          (read-macrolet ((literal-char (esrap-literal-char-reader char-reader))
                          (literal-string (esrap-literal-string-reader string-reader))
                          (character-ranges (esrap-character-ranges char-reader)))
            ,@body)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro-enhance::def-*!-symbol-p c)
  (defun parse-c!-symbol (sym)
    (cl-ppcre:register-groups-bind (second third)
	("^C!-([^-]+)(.*)" (string sym))
      (values (intern (concatenate 'string "C!-" second))
	      (if (string= "" third)
		  nil
		  (subseq third 1))))))
      

(defmacro with-esrap-variable-transformer (&body body)
  `(let ((*variable-transformer* (lambda (sym)
				   (declare (special c!-vars))
				   (if (c!-symbol-p sym)
				       (multiple-value-bind (var-name rule-name) (parse-c!-symbol sym)
					 ;; (format t "Var-name is : ~a, rule-name is : ~a~%" var-name rule-name)
					 (if rule-name
					     (progn (setf (gethash var-name c!-vars) t)
						    `(setq ,var-name
							   (descend-with-rule ',(intern rule-name))))
					     (fail-transform)))
				       ;; KLUDGE to not parse lambda-lists in defrule args
				       (if (equal "CHARACTER" (string sym))
					   `(descend-with-rule 'character nil)
					   `(descend-with-rule ',sym))))))
	 ,@body))

(defun! make-rule-lambda (name args body)
  (multiple-value-bind (reqs opts rest kwds allow-other-keys auxs kwds-p) (parse-ordinary-lambda-list args)
    (declare (ignore kwds))
    (if kwds-p (error "&KEY arguments are not supported"))
    (if allow-other-keys (error "&ALLOW-OTHER-KEYS is not supported"))
    (if auxs (error "&AUX variables are not supported, use LET"))
    `(named-lambda ,(intern (strcat "ESRAP-" name)) (text position end ,@args)
       (let ((,g!-position position))
	 (declare (ignorable ,g!-position))
	 (symbol-macrolet ((match-start ,g!-position)
			   (match-end position))
	   (with-cached-result (,name position text ,@reqs
				      ,@(if rest
					    `(,rest)
					    (iter (for (opt-name opt-default opt-supplied-p) in opts)
						  (collect opt-name)
						  (if opt-supplied-p
						      (collect opt-supplied-p)))))
	     (values (progn ,@body)
		     position)))))))


(defmacro!! defrule (name args &body body &environment env)
    (with-esrap-reader-context
      (call-next-method))
  (with-esrap-variable-transformer
    (let ((c!-vars (make-hash-table)))
      (declare (special c!-vars))
      ;; TODO: bug - C!-vars values are kept between different execution of a rule!
      (let ((pre-body (macroexpand-cc-all-transforming-undefs
		       (make-rule-lambda name args body)
		       :env env)))
	`(setf (gethash ',name *rules*)
	       (let ,(iter (for (key nil) in-hashtable c!-vars)
			   (collect key))
		 ,pre-body))))))

(defmacro! make-result (result &optional (length 0))
  ;; We must preserve the semantics, that computation of results occurs before increment of position
  `(let ((,g!-result ,result))
     (incf position ,length)
     (values ,g!-result position)))


(defmacro! || (&rest clauses)
  `(multiple-value-bind (,g!-result ,g!-position)
       ;; All this tricky business with BLOCK just for automatic POSITION tracking.
       (block ,g!-ordered-choice
         (let (,g!-parse-errors)
           ,@(mapcar (lambda (clause)
                       `(handler-case (let ((position position))
                                        ;; (format t "Im in ordered choice~%")
                                        (return-from ,g!-ordered-choice (values ,clause position)))
                          (simple-esrap-error (e) (push e ,g!-parse-errors))))
                     clauses)
           (fail-parse (joinl "~%"
                              (mapcar (lambda (x)
                                        (slot-value x 'reason))
                                      (nreverse ,g!-parse-errors))))))
     (setf position ,g!-position)
     ,g!-result))
  

(defmacro ! (expr)
  "Succeeds, whenever parsing of EXPR fails. Does not consume, returns NIL, for compatibility with TEXT"
  `(progn (let ((position position))
            (handler-case ,expr
              (simple-esrap-error () nil)
              (:no-error (result &optional position)
                (declare (ignore result position))
                (fail-parse "Clause under non-consuming negation succeeded."))))
          (make-result nil 0)))

(defmacro !! (expr)
  "Succeeds, whenever parsing of EXPR fails. Consumes, assumes than EXPR parses just one character."
  `(progn (let ((position position))
            (handler-case ,expr
              (simple-esrap-error () nil)
              (:no-error (result &optional position)
                (declare (ignore result position))
                (fail-parse "Clause under consuming negation succeeded."))))
          (if (equal end position)
              (fail-parse "Reached EOF while trying to consume character.")
              (make-result (char text position) 1))))

(defmacro! times (subexpr &key from upto exactly)
  (flet ((frob (condition)
           `(let (,g!-result)
              (iter ,(if (or upto exactly)
                         `(for ,g!-i from 1 to ,(or upto exactly)))
                    (multiple-value-bind (,g!-subresult ,g!-position)
                        (handler-case (let ((position position))
                                        (values ,subexpr position))
                          (simple-esrap-error () (finish)))
                      (if-first-time nil
                                     (if (equal ,g!-position position)
                                         (terminate)))
                      (push ,g!-subresult ,g!-result)
                      (setf position ,g!-position))
                    (finally (if ,condition
                                 (return (make-result (nreverse ,g!-result)))
                                 (fail-parse "Greedy repetition failed.")))))))
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
  `(let ((,g!-it ,subexpr))
     (if (funcall ,predicate ,g!-it)
         ,g!-it
         (fail-parse "Predicate test failed"))))

(defmacro progm (start meat end)
  "Prog Middle."
  `(progn ,start (prog1 ,meat ,end)))

(defmacro! ? (subexpr)
  `(multiple-value-bind (,g!-result ,g!-position)
       (block ,g!-?
         (let ((position position))
           (handler-case ,subexpr
             (simple-esrap-error () (values nil nil))
             (:no-error (result) (return-from ,g!-? (make-result result))))))
     (when ,g!-position
       (setf position ,g!-position))
     ,g!-result))

(defmacro & (subexpr)
  `(make-result (let ((position position))
                  ,subexpr)))

(defmacro -> (subexpr)
  (if (and (symbolp subexpr) (equal (string subexpr) "EOF"))
      `(if (equal end position)
           (make-result nil)
           (fail-parse "not at end-of-file"))
      `(progn (let ((position position))
		,subexpr)
	      (make-result nil))))

(defmacro! <- (subexpr)
  (if (and (symbolp subexpr) (equal (string subexpr) "SOF"))
      `(if (equal 0 position)
           (make-result nil)
           (fail-parse "not at start-of-file"))
      `(let ((,g!-old-position position)
             (position (1- position)))
         (let ((,g!-result ,subexpr))
           (if (equal ,g!-old-position position)
               (make-result nil)
               (fail-parse "Parsing of subexpr took more than 1 char."))))))

(defmacro! cond-parse (&rest clauses)
  `(|| ,@(mapcar (lambda (clause)
                   `(progn ,@clause))
                 clauses)))
