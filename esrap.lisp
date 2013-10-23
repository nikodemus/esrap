;;;; ESRAP -- a packrat parser for Common Lisp
;;;; by Nikodemus Siivola, 2007-2012
;;;;
;;;; Homepage and documentation:
;;;;
;;;;   http://nikodemus.github.com/esrap/
;;;;
;;;; References:
;;;;
;;;;   * Bryan Ford, 2002, "Packrat Parsing: a Practical Linear Time
;;;;     Algorithm with Backtracking".
;;;;     http://pdos.csail.mit.edu/~baford/packrat/thesis/
;;;;
;;;; Licence:
;;;;
;;;;  Permission is hereby granted, free of charge, to any person
;;;;  obtaining a copy of this software and associated documentation files
;;;;  (the "Software"), to deal in the Software without restriction,
;;;;  including without limitation the rights to use, copy, modify, merge,
;;;;  publish, distribute, sublicense, and/or sell copies of the Software,
;;;;  and to permit persons to whom the Software is furnished to do so,
;;;;  subject to the following conditions:
;;;;
;;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;;  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;;  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;;  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;;  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :esrap)


;;; Expression destructuring and validation

;;; TODO: Probably this won't be needed, once liquification is complete
(defmacro with-expression ((expr lambda-list) &body body)
  (let* ((type (car lambda-list))
         (car-var (gensym "CAR"))
         (fixed-list (cons car-var (cdr lambda-list))))
    (once-only (expr)
      `(destructuring-bind ,fixed-list ,expr
         ,(if (eq t type)
              `(declare (ignore ,car-var))
              `(unless (eq ',type ,car-var)
                 (error "~S-expression expected, got: ~S" ',type ,expr)))
         (locally ,@body)))))


;;; RESULT REPRESENTATION
;;;
;;; We always return a result -- ERROR-RESULT for failed parses, and
;;; RESULT for successes.
;;;
;;; We implement a simple lazy evaluation for the productions. This is
;;; used to perform semantic actions only when necessary -- either
;;; when we call a semantic predicate or once parse has finished.

(defstruct error-result)

(defstruct (inactive-rule (:include error-result))
  name)

(defstruct (failed-parse (:include error-result))
  ;; Expression that failed to match.
  expression
  ;; Position at which match was attempted.
  (position (required-argument) :type array-index)
  ;; A nested error, closer to actual failure site.
  detail)

(defstruct (result (:constructor %make-result))
  ;; Either a list of results, whose first element is the production, or a
  ;; function to call that will return the production.
  %production
  ;; Position after the match.
  (position (required-argument) :type array-index))

(defmacro make-result (&rest arguments &key production &allow-other-keys)
  (if production
      (let ((args (copy-list arguments)))
        (remf args :production)
        `(%make-result ,@args
                       :%production ,(if (symbolp production)
                                         `(list ,production)
                                         `(lambda () ,production))))
      `(%make-result ,@arguments)))

(defun result-production (result)
  (let ((thunk (result-%production result)))
    (if (functionp thunk)
        (let ((value (funcall thunk)))
          (setf (result-%production result) (list value))
          value)
        (car thunk))))

;;; MAIN INTERFACE

(defun parse (expression text &key (start 0) end junk-allowed)
  "Parses TEXT using EXPRESSION from START to END. Incomplete parses
are allowed only if JUNK-ALLOWED is true."
  ;; There is no backtracking in the toplevel expression -- so there's
  ;; no point in compiling it as it will be executed only once -- unless
  ;; it's a constant, for which we have a compiler-macro.
  (let ((end (or end (length text))))
    (process-parse-result
     (let ((*cache* (make-cache)))
       (eval-expression expression text start end))
     text
     end
     junk-allowed)))

;; (define-compiler-macro parse (&whole form expression &rest arguments
;;                               &environment env)
;;   (if (constantp expression env)
;;       (with-gensyms (expr-fun)
;;         `(let ((,expr-fun (load-time-value (compile-expression ,expression))))
;;            ;; This inline-lambda here provides keyword defaults and
;;            ;; parsing, so the compiler-macro doesn't have to worry
;;            ;; about evaluation order.
;;            ((lambda (text &key (start 0) end junk-allowed)
;;               (let ((*cache* (make-cache))
;;                     (end (or end (length text))))
;;                 (process-parse-result
;;                  (funcall ,expr-fun text start end)
;;                  text
;;                  end
;;                  junk-allowed)))
;;             ,@arguments)))
;;       form))

(defun process-parse-result (result text end junk-allowed)
  (if (error-result-p result)
      (if junk-allowed
          (values nil 0)
          (if (failed-parse-p result)
              (labels ((expressions (e)
                         (when e
                           (cons (failed-parse-expression e)
                                 (expressions (failed-parse-detail e))))))
                (let ((expressions (expressions result)))
                  (simple-esrap-error text (failed-parse-position result)
                                      "Could not parse subexpression ~S when ~
                                       parsing~2&~< Expression ~S~@{~&    ~
                                       Subexpression ~S~}~:>"
                                      (lastcar expressions)
                                      expressions)))
              (simple-esrap-error text nil "rule ~S not active"
                                  (inactive-rule-name result))))
      (let ((position (result-position result)))
        (values (result-production result)
                (when (< position end)
                  (if junk-allowed
                      position
                      (simple-esrap-error text position "Incomplete parse.")))))))

(defmacro! defrule (&whole form symbol expression check)
  "Define SYMBOL as a nonterminal, using EXPRESSION as associated the parsing expression.
Rule is only succeeds, if CHECK evaluates to true."
  (let ((transform nil)
        (around nil)
        (guard t)
        (condition t)
        (guard-seen nil))
    (when options
      (dolist (option options)
        (flet ((set-transform (trans)
                 (if transform
                     (error "Multiple transforms in DEFRULE:~% ~S" form)
                     (setf transform trans)))
               (set-guard (expr test)
                 (if guard-seen
                     (error "Multiple guards in DEFRULE:~% ~S" form)
                     (setf guard-seen t
                           guard expr
                           condition test))))
          (destructuring-ecase option
            ((:when expr)
             (when (cddr option)
               (error "Multiple expressions in a :WHEN:~% ~S" form))
             (if (constantp expr)
                 (if (eval expr)
                     (set-guard expr t)
                     (set-guard expr nil))
                 (set-guard expr `(lambda () ,expr))))
            ((:constant value)
             (setf transform `(constantly ,value)))
            ((:concat value)
             (note-deprecated :concat :text)
             (when value
               (setf transform '#'text/bounds)))
            ((:text value)
             (when value
               (setf transform '#'text/bounds)))
            ((:identity value)
             (when value
               (setf transform '#'identity/bounds)))
            ((:lambda lambda-list &body forms)
             (multiple-value-bind (lambda-list start end ignore)
                 (parse-lambda-list-maybe-containing-&bounds lambda-list)
               (setf transform
                     `(lambda (,@lambda-list ,start ,end)
                        (declare (ignore ,@ignore))
                        ,@forms))))
            ((:function designator)
             (setf transform `(lambda/bounds (function ,designator))))
            ((:destructure lambda-list &body forms)
             (multiple-value-bind (lambda-list start end ignore)
                 (parse-lambda-list-maybe-containing-&bounds lambda-list)
               (setf transform
                     (with-gensyms (production)
                       `(lambda (,production ,start ,end)
                          (declare (ignore ,@ignore))
                          (destructuring-bind ,lambda-list ,production
                            ,@forms))))))
            ((:around lambda-list &body forms)
             (multiple-value-bind (lambda-list start end ignore)
                 (parse-lambda-list-maybe-containing-&bounds lambda-list)
               (assert (null lambda-list))
               (setf around `(lambda (,start ,end transform)
                               (declare (ignore ,@ignore)
                                        (function transform))
                               (flet ((call-transform ()
                                        (funcall transform)))
                                 ,@forms)))))
	    ((:wrap-around &body forms)
	     (setf around `(lambda (,e!-wrapper ,e!-parser) ; should change to g!-syms here
			     (declare (ignorable ,e!-wrapper ,e!-parser))
			     (flet ((,e!-call-parser ()
				      (funcall ,e!-parser)))
			       ,@forms))))))))
    `(eval-when (:load-toplevel :execute)
       (add-rule ',symbol (make-instance 'rule
                                         :expression ',expression
                                         :guard-expression ',guard
                                         :transform ,(or transform '#'identity/bounds)
                                         :around ,around
                                         :condition ,condition)))))

(defun add-rule (symbol rule)
  "Associates RULE with the nonterminal SYMBOL. Signals an error if the
rule is already associated with a nonterminal. If the symbol is already
associated with a rule, the old rule is removed first."
  ;; FIXME: This needs locking and WITHOUT-INTERRUPTS.
  (check-type symbol nonterminal)
  (when (rule-symbol rule)
    (error "~S is already associated with the nonterminal ~S -- remove it first."
           rule (rule-symbol rule)))
  (let* ((cell (ensure-rule-cell symbol))
	 (function (compile-rule symbol
                                 (rule-expression rule)
                                 (rule-condition rule)
                                 (rule-transform rule)
                                 (rule-around rule)))
         (function (lambda (text position end)
		     (funcall function text position end)))
         (trace-info (cell-trace-info cell)))
    (set-cell-info cell function rule)
    (setf (cell-trace-info cell) nil)
    (setf (slot-value rule '%symbol) symbol)
    (when trace-info
      (trace-rule symbol :break (second trace-info)))
    symbol))

(defun find-rule (symbol)
  "Returns rule designated by SYMBOL, if any. Symbol must be a nonterminal
symbol."
  (check-type symbol nonterminal)
  (let ((cell (find-rule-cell symbol)))
    (when cell
      (cell-rule cell))))

(defun remove-rule (symbol &key force)
  "Makes the nonterminal SYMBOL undefined. If the nonterminal is defined an
already referred to by other rules, an error is signalled unless :FORCE is
true."
  (check-type symbol nonterminal)
  ;; FIXME: Lock and WITHOUT-INTERRUPTS.
  (let* ((cell (find-rule-cell symbol))
         (rule (cell-rule cell))
         (trace-info (cell-trace-info cell)))
    (when cell
      (flet ((frob ()
               (set-cell-info cell (undefined-rule-function symbol) nil)
               (when trace-info
                 (setf (cell-trace-info cell) (list (cell-%info cell) (second trace-info))))
               (when rule
                 (detach-rule rule))))
        (cond ((and rule (cell-referents cell))
               (unless force
                 (error "Nonterminal ~S is used by other nonterminal~P:~% ~{~S~^, ~}"
                        symbol (length (cell-referents cell)) (cell-referents cell)))
               (frob))
              ((not (cell-referents cell))
               (frob)
               ;; There are no references to the rule at all, so
               ;; we can remove the cell.
               (unless trace-info
                 (delete-rule-cell symbol)))))
      rule)))

(defvar *trace-level* 0)

(defvar *trace-stack* nil)

(defun trace-rule (symbol &key recursive break)
  "Turn on tracing of nonterminal SYMBOL. If RECURSIVE is true, turn
on tracing for the whole grammar rooted at SYMBOL. If BREAK is true,
break is entered when the rule is invoked."
  (unless (member symbol *trace-stack* :test #'eq)
    (let ((cell (find-rule-cell symbol)))
      (unless cell
        (error "Undefined rule: ~S" symbol))
      (when (cell-trace-info cell)
        (let ((*trace-stack* nil))
          (untrace-rule symbol)))
      (let ((fun (cell-function cell))
            (rule (cell-rule cell))
            (info (cell-%info cell)))
        (set-cell-info cell
                       (lambda (text position end)
                         (when break
                           (break "rule ~S" symbol))
                         (let ((space (make-string *trace-level* :initial-element #\space))
                               (*trace-level* (+ 1 *trace-level*)))
                           (format *trace-output* "~&~A~D: ~S ~S? ~%"
                                   space *trace-level* symbol position)
                           (finish-output *trace-output*)
                           (let ((result (funcall fun text position end)))
                             (if (error-result-p result)
                                 (format *trace-output* "~&~A~D: ~S -|~%"
                                         space *trace-level* symbol)
                                 (format *trace-output* "~&~A~D: ~S ~S-~S -> ~S~%"
                                         space *trace-level* symbol
                                         position
                                         (result-position result)
                                         (result-production result)))
                             (finish-output *trace-output*)
                             result)))
                       rule)
        (setf (cell-trace-info cell) (list info break)))
      (when recursive
        (let ((*trace-stack* (cons symbol *trace-stack*)))
          (dolist (dep (%rule-direct-dependencies (cell-rule cell)))
            (trace-rule dep :recursive t :break break))))
      t)))

(defun untrace-rule (symbol &key recursive break)
  "Turn off tracing of nonterminal SYMBOL. If RECURSIVE is true, untraces the
whole grammar rooted at SYMBOL. BREAK is ignored, and is provided only for
symmetry with TRACE-RULE."
  (declare (ignore break))
  (unless (member symbol *trace-stack* :test #'eq)
    (let ((cell (find-rule-cell symbol)))
      (unless cell
        (error "Undefined rule: ~S" symbol))
      (let ((trace-info (cell-trace-info cell)))
        (when trace-info
          (setf (cell-%info cell) (car trace-info)
                (cell-trace-info cell) nil))
        (when recursive
          (let ((*trace-stack* (cons symbol *trace-stack*)))
            (dolist (dep (%rule-direct-dependencies (cell-rule cell)))
              (untrace-rule dep :recursive t))))))
    nil))

(defun rule-expression (rule)
  "Return the parsing expression associated with the RULE."
  (slot-value rule '%expression))

(defun (setf rule-expression) (expression rule)
  "Modify RULE to use EXPRESSION as the parsing expression. The rule must be
detached beforehand."
  (let ((name (rule-symbol rule)))
    (when name
      (error "~@<Cannot change the expression of an active rule, ~
              remove ~S first, or use CHANGE-RULE.~:@>"
             name))
    (setf (slot-value rule '%expression) expression)))

(defun change-rule (symbol expression)
  "Modifies the nonterminal SYMBOL to use EXPRESSION instead. Temporarily
removes the rule while it is being modified."
  (let ((rule (remove-rule symbol :force t)))
    (unless rule
      (error "~S is not a defined rule." symbol))
    (setf (rule-expression rule) expression)
    (add-rule symbol rule)))

(defun symbol-length (x)
  (length (symbol-name x)))

(defun describe-grammar (symbol &optional (stream *standard-output*))
  "Prints the grammar tree rooted at nonterminal SYMBOL to STREAM for human
inspection."
  (check-type symbol nonterminal)
  (let ((rule (find-rule symbol)))
    (cond ((not rule)
           (format stream "Symbol ~S is not a defined nonterminal." symbol))
          (t
           (format stream "~&Grammar ~S:~%" symbol)
           (multiple-value-bind (defined undefined) (rule-dependencies rule)
             (let ((length
                    (+ 4 (max (reduce #'max (mapcar #'symbol-length defined)
                                      :initial-value 0)
                              (reduce #'max (mapcar #'symbol-length undefined)
                                      :initial-value 0)))))
               (format stream "~3T~S~VT<- ~S~@[ : ~S~]~%"
                       symbol length (rule-expression rule)
                       (when (rule-condition rule)
                         (rule-guard-expression rule)))
               (when defined
                 (dolist (s defined)
                   (let ((dep (find-rule s)))
                     (format stream "~3T~S~VT<- ~S~@[ : ~S~]~%"
                            s length (rule-expression dep)
                            (when (rule-condition rule)
                              (rule-guard-expression rule))))))
               (when undefined
                 (format stream "~%Undefined nonterminal~P:~%~{~3T~S~%~}"
                         (length undefined) undefined))))))))

;;; COMPILING RULES

(defvar *current-rule* nil)

(defun compile-rule (symbol expression condition transform around)
  (declare (type (or boolean function) condition transform around))
  (macrolet ((cur-parse-failed (&optional (result-var 'result))
	       `(make-failed-parse
		 :expression symbol
		 :position (if (failed-parse-p ,result-var)
			       (failed-parse-position ,result-var)
			       position)
		 :detail ,result-var))
	     (call-transform (&optional (result-var 'result))
	       `(funcall transform
			 (result-production ,result-var)
			 position
			 (result-position ,result-var)))
	     (conditionally-exec (name &body body)
	       `(if (eq t condition)
		    (named-lambda ,name (text position end)
		      (with-cached-result (symbol position text)
			,@body))
		    (named-lambda ,(intern (strcat "CONDITIONAL-" name)) (text position end)
		     (with-cached-result (symbol position text)
		       (if (funcall condition)
			   (progn ,@body)
			   rule-not-active)))))
	     (make-result-evenly (result-var &optional (production '(call-transform)))
	       `(make-result :position (result-position ,result-var)
			     :production ,production)))
    (let ((*current-rule* symbol))
      ;; Must bind *CURRENT-RULE* before compiling the expression!
      (if (and (consp expression)
	       (symbolp (car expression))
	       (equal (string (car expression)) "WRAP"))
	  (destructuring-bind (wrap wrapper wrappie) expression
	    (declare (ignore wrap))
	    (let* ((func-wrapper (compile-expression wrapper))
		   (rule-not-active (when condition (make-inactive-rule :name symbol))))
	      (cond ((not condition)
		     (named-lambda inactive-rule (text position end)
		       (declare (ignore text position end))
		       rule-not-active))
		    ((not (and transform around))
		     (error "When specifying WRAP rule both TRANSFORM and AROUND should be present."))
		    (t (flet ((exec-rule/wrap (text position end)
				(let ((wrapper-result (funcall func-wrapper text position end)))
				  (if (error-result-p wrapper-result)
				      (cur-parse-failed wrapper-result)
				      (funcall around
					       (result-production wrapper-result)
					       (lambda ()
						 (let* ((func-wrappie (compile-expression wrappie))
							(wrappie-result (funcall func-wrappie text
										 (result-position wrapper-result)
										 end)))
						   (if (error-result-p wrappie-result)
						       (cur-parse-failed wrappie-result)
						       (let ((production (call-transform wrappie-result)))
							 (make-result-evenly wrappie-result
									     (values production)))))))))))
			 (conditionally-exec rule/wrap
			   (exec-rule/wrap text position end)))))))
	  (let* ((function (compile-expression expression))
		 (rule-not-active (when condition (make-inactive-rule :name symbol))))
	    (cond ((not condition)
		   (named-lambda inactive-rule (text position end)
		     (declare (ignore text position end))
		     rule-not-active))
		  (transform
		   (flet ((exec-rule/transform (text position end)
			    (let ((result (funcall function text position end)))
			      (if (error-result-p result)
				  (cur-parse-failed)
				  (if around
				      (make-result-evenly result
							  (flet ((call-rule ()
								   (call-transform)))
							    (funcall around
								     position
								     (result-position result)
								     #'call-rule)))
				      (make-result-evenly result))))))
		     (conditionally-exec rule/transform
		       (exec-rule/transform text position end))))
		  (t (conditionally-exec rule
		       (funcall function text position end)))))))))

;;; EXPRESSION COMPILER & EVALUATOR

(defun invalid-expression-error (expression)
  (error "Invalid expression: ~S" expression))

(defun validate-character-range (range)
  (or
    (characterp range)
    (and
      (consp range)
      (consp (cdr range))
      (characterp (car range))
      (characterp (cadr range))
      (null (cddr range)))))

(defun validate-expression (expression)
  (or (typecase expression
        ((eql character)
         t)
        (terminal
         t)
        (nonterminal
         t)
        (cons
         (case (car expression)
           ((and or wrap)
            (and (every #'validate-expression (cdr expression)) t))
           ((nil)
            nil)
           (string
            (and (cdr expression) (not (cddr expression))
                 (typep (second expression) 'array-length)))
           (character-ranges
            (and (every #'validate-character-range (cdr expression)) t))
	   (* (and (>= (length expression) 2)
		   (validate-expression (car (last expression)))))
	   (cond (and (every (lambda (x)
			       (and (every #'validate-expression x) t))
			     (cdr expression))
		      t))
	   (tag (and (equal (length expression) 3)
		     (keywordp (cadr expression))
		     (validate-expression (caddr expression))))
           (t
            (and (symbolp (car expression))
                 (cdr expression) (not (cddr expression))
                 (validate-expression (second expression))))))
        (t
         nil))
      (invalid-expression-error expression)))

(defun %expression-dependencies (expression seen)
  (etypecase expression
    ((member character)
     seen)
    (terminal
     seen)
    (nonterminal
     (if (member expression seen :test #'eq)
         seen
         (let ((rule (find-rule expression))
               (seen (cons expression seen)))
           (if rule
               (%expression-dependencies (rule-expression rule) seen)
               seen))))
    (cons
     (case (car expression)
       ((string character-ranges)
        seen)
       ((and or)
        (dolist (subexpr (cdr expression) seen)
          (setf seen (%expression-dependencies subexpr seen))))
       ((* + ? & !)
        (%expression-dependencies (second expression) seen))
       (t
        (%expression-dependencies (second expression) seen))))))

(defun %expression-direct-dependencies (expression seen)
  (etypecase expression
    ((member character)
     seen)
    (terminal
     seen)
    (nonterminal
     (cons expression seen))
    (cons
     (case (car expression)
       (string
        seen)
       ((and or)
        (dolist (subexpr (cdr expression) seen)
          (setf seen (%expression-direct-dependencies subexpr seen))))
       ((* + ? & !)
        (%expression-direct-dependencies (second expression) seen))
       (t
        (%expression-direct-dependencies (second expression) seen))))))

(defun eval-expression (expression text position end)
  (macrolet ((frob ((&rest clauses) &body body)
	       `(case (car expression)
		  ,@(mapcar (lambda (clause)
			      (let ((clause (if (atom clause) `(,clause) clause)))
				`(,(car clause) (,(sb-int:symbolicate "EVAL-" (or (cadr clause) (car clause)))
						  expression text position end))))
			    clauses)
		  ,@body)))
    (typecase expression
      ((eql character)
       (eval-character text position end))
      (terminal
       (if (consp expression)
	   (eval-terminal (string (second expression)) text position end nil)
	   (eval-terminal (string expression) text position end t)))
      (nonterminal
       (eval-nonterminal expression text position end))
      (cons
       (frob (string (and sequence) (or ordered-choice) (not negation)
		     (+ greedy-positive-repetition) (? optional) (& followed-by) (-> followed-by-not-gen)
		     (<- preceded-by-not-gen) (! not-followed-by) character-ranges cond first tag)
	     (* (cond ((equal (length expression) 2) (eval-greedy-repetition expression text position end))
		      (t (eval-times expression text position end))))
	     (t
	      (if (symbolp (car expression))
		  (eval-semantic-predicate expression text position end)
		  (invalid-expression-error expression)))))
      (t
       (invalid-expression-error expression)))))

(defun compile-expression (expression)
  (macrolet ((frob ((&rest clauses) &body body)
	       `(case (car expression)
		  ,@(mapcar (lambda (clause)
			      (let ((clause (if (atom clause) `(,clause) clause)))
				`(,(car clause) (,(sb-int:symbolicate "COMPILE-" (or (cadr clause) (car clause)))
						  expression))))
			    clauses)
		  ,@body)))
    (etypecase expression
      ((eql character)
       (compile-character))
      (terminal
       (if (consp expression)
	   (compile-terminal (string (second expression)) nil)
	   (compile-terminal (string expression) t)))
      (nonterminal
       (compile-nonterminal expression))
      (cons
       (frob (string (and sequence) (or ordered-choice) (not negation)
		     (+ greedy-positive-repetition) (? optional) (& followed-by) (-> followed-by-not-gen)
		     (<- preceded-by-not-gen) (! not-followed-by) character-ranges cond first tag)
	     (* (cond ((equal (length expression) 2) (compile-greedy-repetition expression))
		      (t (compile-times expression))))
	     (t
	      (if (symbolp (car expression))
		  (compile-semantic-predicate expression)
		  (invalid-expression-error expression)))))
      (t
       (invalid-expression-error expression)))))

;;; Characters and strings

(defmacro! any-string (length)
  (let ((expression '(any-string ,length))
	(limit (+ ,length position)))
    (if (<= limit end)
        (make-result
         :production (subseq text position limit)
         :position limit)
	(fail "Unable to parse any string of specified length."))))

(define-symbol-macro character
    (let ((expression 'character))
      (if (< position end)
	  (make-result
	   :production (char text position)
	   :position (1+ position))
	  (fail "EOF reached while trying to parse character."))))


;;; Terminals
;;;
;;; FIXME: It might be worth it to special-case terminals of length 1.

;;; #]foo[ denotes literal string terminal "foo"
(set-dispatch-macro-character #\# #\]
			      (lambda (stream subchar arg)
				(declare (ignore subchar arg))
				`(terminal ,(sb-impl::read-string stream #\[) nil)))

(set-dispatch-macro-character #\# #\/ (let ((f (get-dispatch-macro-character #\# #\\)))
					(lambda (stream subchar arg)
					  (let ((char (funcall f stream subchar arg)))
					    `(terminal ,char nil)))))

(defmacro terminal (terminal case-insensitive)
  (let ((length (length terminal))
	(terminal (string terminal)))
    `(if (and (<= (+ ,length position) end)
	      ,(if case-insensitive
		   `(string-equal ,terminal text :start2 position :end2 (+ position ,length))
		   `(string= ,terminal text :start2 position :end2 (+ position ,length))))
	 (make-result
	  :position (+ ,length position)
	  :production '(terminal ,terminal))
	 (fail))))

(defmacro! ~ (terminal)
  "Expands into case-insensitive terminal checking."
  `(terminal ,terminal t))

;;; Nonterminals

(defparameter *eval-nonterminals* nil)

(defun eval-nonterminal (symbol text position end)
  (if *eval-nonterminals*
      (eval-expression (rule-expression (find-rule symbol)) text position end)
      (funcall (cell-function (ensure-rule-cell symbol)) text position end)))

(defun compile-nonterminal (symbol)
  (let ((cell (reference-rule-cell symbol *current-rule*)))
    (declare (rule-cell cell))
    (named-lambda compile-nonterminal (text position end)
      (funcall (cell-function cell) text position end))))

;;; Sequences
;;;
;;; FIXME: It might be better if we actually chained the closures
;;; here, instead of looping over them -- benchmark first, though.

(defmacro! fail (detail)
  `(error 'parse-error
	  :expression expression
	  :position position
	  :detail ,detail))
  

(defmacro! && (&rest subexpressions)
  `(let ((,g!-subexprs (list ,@(mapcar (lambda (x) `(lambda ()
						      (let ((expression ',x))
							,x))
					       subexpressions)))))
     (iter (for ,g!-subexpr in ,g!-subexprs)
	   (let ((,g!-result (funcall ,g!-subexpr)))
	     (if (error-result-p ,g!-result)
		 (fail ,g!-result)
		 (progn (setf position (result-position ,g!-result))
			(collect (result-production ,g!-result))))))))
	   
;;; Ordered choices

(defun eval-ordered-choice (expression text position end)
  (with-expression (expression (or &rest subexprs))
    (let (last-error)
      (dolist (expr subexprs
               (make-failed-parse
                :expression expression
                :position (if (failed-parse-p last-error)
                              (failed-parse-position last-error)
                              position)
                :detail last-error))
        (let ((result (eval-expression expr text position end)))
          (if (error-result-p result)
              (when (or (and (not last-error)
                             (or (inactive-rule-p result)
                                 (< position (failed-parse-position result))))
                        (and last-error
                             (failed-parse-p result)
                             (or (inactive-rule-p last-error)
                                 (< (failed-parse-position last-error)
                                    (failed-parse-position result)))))
                (setf last-error result))
              (return result)))))))

(defun compile-ordered-choice (expression)
  (with-expression (expression (or &rest subexprs))
    (let ((type :characters)
          (canonized nil))
      (dolist (sub subexprs)
        (when (typep sub '(or character string))
          (let* ((this (string sub))
                 (len (length this)))
            (unless (some (lambda (seen)
                            (not
                             ;; Check for "FOO" followed by "FOOBAR" -- the
                             ;; latter would never match, but it's an easy mistake to make.
                             (or (mismatch this seen :end1 (min (length seen) len))
                                 (warn "Prefix ~S before ~S in an ESRAP OR expression."
                                       seen this))))
                        canonized)
              (push this canonized))))
        (case type
          (:general)
          (:strings
           (unless (typep sub '(or character string))
             (setf type :general)))
          (:characters
           (unless (typep sub '(or character (string 1)))
             (if (typep sub 'string)
                 (setf type :strings)
                 (setf type :general))))))
      ;; FIXME: Optimize case-insensitive terminals as well.
      (ecase type
        (:characters
         ;; If every subexpression is a length 1 string, we can represent the whole
         ;; choice with a single string.
         (let ((choices (apply #'concatenate 'string canonized)))
           (named-lambda compiled-character-choice (text position end)
             (let ((c (and (< position end) (find (char text position) choices))))
               (if c
                   (make-result :position (+ 1 position)
                                :production (string c))
                   (make-failed-parse
                    :expression expression
                    :position position))))))
        (:strings
         ;; If every subexpression is a string, we can represent the whole choice
         ;; with a list of strings.
         (let ((choices (nreverse canonized)))
           (named-lambda compiled-character-choice (text position end)
             (dolist (choice choices
                      (make-failed-parse
                       :expression expression
                       :position position))
               (let ((len (length choice)))
                 (when (match-terminal-p choice len text position end t)
                   (return
                     (make-result :position (+ len position)
                                  :production choice))))))))
        (:general
         ;; In the general case, compile subexpressions and call.
         (let ((functions (mapcar #'compile-expression subexprs)))
             (named-lambda compiled-ordered-choice (text position end)
               (let (last-error)
                 (dolist (fun functions
                          (make-failed-parse
                           :expression expression
                           :position (if (and last-error
                                              (failed-parse-p last-error))
                                         (failed-parse-position last-error)
                                         position)
                           :detail last-error))
                   (let ((result (funcall fun text position end)))
                     (if (error-result-p result)
                         (when (or (and (not last-error)
                                        (or (inactive-rule-p result)
                                            (< position (failed-parse-position result))))
                                   (and last-error
                                        (failed-parse-p result)
                                        (or (inactive-rule-p last-error)
                                            (< (failed-parse-position last-error)
                                               (failed-parse-position result)))))
                           (setf last-error result))
                         (return result))))))))))))

;;; Negations

(defun exec-negation (fun expr text position end)
  (if (and (< position end)
           (error-result-p (funcall fun text position end)))
      (make-result
       :position (1+ position)
       :production (char text position))
      (make-failed-parse
       :expression expr
       :position position)))

(defun eval-negation (expression text position end)
  (with-expression (expression (not subexpr))
    (flet ((eval-sub (text position end)
             (eval-expression subexpr text position end)))
      (declare (dynamic-extent #'eval-sub))
      (exec-negation #'eval-sub expression text position end))))

(defun compile-negation (expression)
  (with-expression (expression (not subexpr))
    (let ((sub (compile-expression subexpr)))
      (named-lambda compiled-negation (text position end)
        (exec-negation sub expression text position end)))))

;;; On-the-fly tagging
(defun eval-tag (expression text position end)
  (funcall (compile-tag expression) text position end))

(defun compile-tag (expression)
  (with-expression (expression (tag keyword subexpr))
    (let ((function (compile-expression subexpr)))
      (named-lambda compiled-tag (text position end)
        (let ((result (funcall function text position end)))
	  (if (error-result-p result)
	      result
	      (make-result
	       :position (result-position result)
	       :production `(,keyword ,(result-production result)))))))))

;;; Greedy repetitions

(defun eval-greedy-repetition (expression text position end)
  (funcall (compile-greedy-repetition expression) text position end))

(defun compile-greedy-repetition (expression)
  (with-expression (expression (* subexpr))
    (let ((function (compile-expression subexpr)))
      (named-lambda compiled-greedy-repetition (text position end)
        (let ((results
               (iter (for result next (funcall function text position end))
		     (until (or (error-result-p result)
				(if-first-time nil
					       (equal (result-position result) position))))
                     (setf position (result-position result))
                     (collect result))))
          (make-result
           :position position
           :production (mapcar #'result-production results)))))))

(defun eval-times (expression text position end)
  (funcall (compile-times expression) text position end))

(defun compile-times (expression)
  (destructuring-bind (from to subexpr)
      (if (equal (length expression) 3)
	  `(0 ,@(cdr expression))
	  (cdr expression))
    (let ((evallee `(let ((function (compile-expression ',subexpr)))
		      (named-lambda compiled-times (text position end)
			(let* ((last nil)
			       (results
				(iter ,@(if to `((for i from 1 to ,to)))
				      (for result next (funcall function text position end))
				      (until (or (error-result-p (setf last result))
						 (if-first-time nil
								(equal (result-position result) position))))
				      (setf position (result-position result))
				      (collect result))))
			  (if (>= (length results) ,from)
			      (make-result
			       :position position
			       :production (mapcar #'result-production results))
			      (make-failed-parse
			       :position position
			       :expression ',expression
			       :detail last)))))))
      (eval evallee))))


;;; Greedy positive repetitions

(defun eval-greedy-positive-repetition (expression text position end)
  (funcall (compile-greedy-positive-repetition expression)
           text position end))

(defun compile-greedy-positive-repetition (expression)
  (with-expression (expression (+ subexpr))
    (let ((function (compile-expression subexpr)))
      (named-lambda compiled-greedy-positive-repetition (text position end)
        (let* ((last nil)
               (results
                (iter (for result next (funcall function text position end))
		      (until (or (error-result-p (setf last result))
				 (if-first-time nil
						(equal (result-position result) position))))
		      (setf position (result-position result))
		      (collect result))))
          (if results
              (make-result
               :position position
               :production (mapcar #'result-production results))
              (make-failed-parse
               :position position
               :expression expression
               :detail last)))))))

;;; Optionals

(defun eval-optional (expression text position end)
  (with-expression (expression (? subexpr))
    (let ((result (eval-expression subexpr text position end)))
      (if (error-result-p result)
          (make-result :position position)
          result))))

(defun compile-optional (expression)
  (with-expression (expression (? subexpr))
    (let ((function (compile-expression subexpr)))
      (named-lambda compiled-optional (text position end)
        (let ((result (funcall function text position end)))
          (if (error-result-p result)
              (make-result :position position)
              result))))))

;;; Followed-by's

(defun eval-followed-by (expression text position end)
  (with-expression (expression (& subexpr))
    (let ((result (eval-expression subexpr text position end)))
      (if (error-result-p result)
          (make-failed-parse
           :position position
           :expression expression
           :detail result)
          (make-result
           :position position
           :production (result-production result))))))

(defun compile-followed-by (expression)
  (with-expression (expression (& subexpr))
    (let ((function (compile-expression subexpr)))
      (named-lambda compiled-followed-by (text position end)
        (let ((result (funcall function text position end)))
          (if (error-result-p result)
              (make-failed-parse
               :position position
               :expression expression
               :detail result)
              (make-result
               :position position
               :production (result-production result))))))))

;;; Followed-by-not-gen's

(defun eval-followed-by-not-gen (expression text position end)
  (with-expression (expression (-> subexpr))
    (let ((result (if (and (symbolp subexpr) (equal (string subexpr) "EOF"))
		      (if (equal position end)
			  (make-result :position position)
			  (make-failed-parse :expression subexpr :position position))
		      (eval-expression subexpr text position end))))
      (if (error-result-p result)
          (make-failed-parse
           :position position
           :expression expression
           :detail result)
          (make-result
           :position position)))))

(defun compile-followed-by-not-gen (expression)
  (with-expression (expression (-> subexpr))
    (let ((function (if (and (symbolp subexpr) (equal (string subexpr) "EOF"))
			(lambda (text position end)
			  (declare (ignore text))
			  (if (equal position end)
			      (make-result :position position)
			      (make-failed-parse :expression subexpr :position position)))
			(compile-expression subexpr))))
      (named-lambda compiled-followed-by-not-gen (text position end)
        (let ((result (funcall function text position end)))
          (if (error-result-p result)
              (make-failed-parse
               :position position
               :expression expression
               :detail result)
              (make-result
               :position position)))))))

;;; Not followed-by's

(defun eval-not-followed-by (expression text position end)
  (with-expression (expression (! subexpr))
    (let ((result (eval-expression subexpr text position end)))
      (if (error-result-p result)
          (make-result
           :position position)
          (make-failed-parse
           :expression expression
           :position position)))))

(defun compile-not-followed-by (expression)
  (with-expression (expression (! subexpr))
    (let ((function (compile-expression subexpr)))
      (named-lambda compiled-not-followed-by (text position end)
        (let ((result (funcall function text position end)))
          (if (error-result-p result)
              (make-result
               :position position)
              (make-failed-parse
               :expression expression
               :position position)))))))

;;; Preceded-by's

(defun eval-preceded-by-not-gen (expression text position end)
  (with-expression (expression (<- subexpr))
    (let ((result (if (and (symbolp subexpr) (equal (string subexpr) "SOF"))
		      (if (equal position 0)
			  (make-result :position position)
			  (make-failed-parse :expression subexpr :position position))
		      (eval-expression subexpr text (1- position) end))))
      (if (or (error-result-p result) (not (equal (result-position result) position)))
          (make-failed-parse
           :position position
           :expression expression
           :detail result)
          (make-result
           :position position)))))

(defun compile-preceded-by-not-gen (expression)
  (with-expression (expression (<- subexpr))
    (let ((function (if (and (symbolp subexpr) (equal (string subexpr) "SOF"))
			(lambda (text position end)
			  (declare (ignore text end))
			  (if (equal position -1)
			      (make-result :position 0)
			      (make-failed-parse :expression subexpr :position position)))
			(compile-expression subexpr))))
      (named-lambda compiled-preceded-by-not-gen (text position end)
        (let ((result (funcall function text (1- position) end)))
          (if (or (error-result-p result) (not (equal (result-position result) position)))
              (make-failed-parse
               :position position
               :expression expression
               :detail result)
              (make-result
               :position position)))))))


;;; Semantic predicates

(defun eval-semantic-predicate (expression text position end)
  (with-expression (expression (t subexpr))
    (let ((result (eval-expression subexpr text position end)))
      (if (error-result-p result)
          (make-failed-parse
           :position position
           :expression expression
           :detail result)
          (let ((production (result-production result)))
            (if (funcall (symbol-function (car expression)) production)
                result
                (make-failed-parse
                 :position position
                 :expression expression)))))))

(defun compile-semantic-predicate (expression)
  (with-expression (expression (t subexpr))
    (let* ((function (compile-expression subexpr))
           (predicate (car expression))
           ;; KLUDGE: Calling via a variable symbol can be slow, and if we
           ;; grab the SYMBOL-FUNCTION here we will not see redefinitions.
           (semantic-function
            (if (eq (symbol-package predicate) (load-time-value (find-package :cl)))
                (symbol-function predicate)
                (compile nil `(lambda (x) (,predicate x))))))
      (named-lambda compiled-semantic-predicate (text position end)
        (let ((result (funcall function text position end)))
          (if (error-result-p result)
              (make-failed-parse
               :position position
               :expression expression
               :detail result)
              (let ((production (result-production result)))
                (if (funcall semantic-function production)
                    result
                    (make-failed-parse
                     :position position
                     :expression expression)))))))))

;;; Character ranges

(defun exec-character-ranges (expression ranges text position end)
  (flet ((oops ()
           (make-failed-parse
            :expression expression
            :position position)))
    (if (< position end)
        (let ((char (char text position)))
          (if (loop for range in ranges
                   do (if (characterp range)
                          (when (char= range char)
                            (return t))
                          (when (char<= (first range) char (second range))
                            (return t))))
             (make-result
              :production char
              :position (1+ position))
             (oops)))
        (oops))))

(defun eval-character-ranges (expression text position end)
  (with-expression (expression (character-ranges &rest ranges))
    (exec-character-ranges expression ranges text position end)))

(defun compile-character-ranges (expression)
  (with-expression (expression (character-ranges &rest ranges))
    (named-lambda compiled-character-ranges (text position end)
      (exec-character-ranges expression ranges text position end))))

(defun eval-cond (expression text position end)
  (funcall (compile-cond expression)
           text position end))

(defun compile-cond (expression)
  (with-expression (expression (cond &rest subexprs))
    (let ((functions (iter (for subexp in subexprs)
			   (collect `(,(if (and (symbolp (car subexp))
						(or (eql (car subexp) 't)
						    (eql (car subexp) 'otherwise)))
					   (lambda (text start end)
					     (declare (ignore text end))
					     (make-result :position start
							  :production (lambda () t)))
					   (compile-expression (car subexp)))
				       ,(compile-expression (cadr subexp)))))))
      (named-lambda compiled-cond (text position end)
	(let (pred-result result last-error)
	  (macrolet ((mark-result-as-last-error (&optional (result-var 'result))
		       `(when (or (and (not last-error)
				       (or (inactive-rule-p ,result-var)
					   (< position (failed-parse-position ,result-var))))
				  (and last-error
				       (failed-parse-p ,result-var)
				       (or (inactive-rule-p last-error)
					   (< (failed-parse-position last-error)
					      (failed-parse-position ,result-var)))))
			  (setf last-error ,result-var))))
	    (iter (for (predicate value-function) in functions)
		  (setf pred-result (funcall predicate text position end))
		  (if (error-result-p pred-result)
		      (mark-result-as-last-error pred-result)
		      (progn (setf result (funcall value-function text
						   (result-position pred-result) end))
			     (if (error-result-p result)
				 (mark-result-as-last-error)
				 (terminate))))
		  (finally (return (if (and result (not (error-result-p result)))
				       result
				       (make-failed-parse
					:expression expression
					:position (if (and last-error
							   (failed-parse-p last-error))
						      (failed-parse-position last-error)
						      position)
					:detail last-error)))))))))))

(defun eval-first (expression text position end)
  (funcall (compile-first expression)
           text position end))

(defun compile-first (expression)
  (with-expression (expression (first subexpr))
    (let ((function (compile-expression subexpr)))
      (named-lambda compiled-first (text position end)
        (let ((result (funcall function text position end)))
          (if (error-result-p result)
	      result
              (make-result
               :position (result-position result)
               :production (car (result-production result)))))))))
			  

(defvar *indentation-hint-table* nil)

(defun hint-slime-indentation ()
  (let* ((swank (find-package :swank))
         (tables (when swank
                   (find-symbol (string '#:*application-hints-tables*) swank))))
    (when tables
      (let ((table (make-hash-table :test #'eq)))
        (setf (gethash 'defrule table)
              '(4 4 &rest (&whole 2 &lambda &body)))
        (set tables (cons table (remove *indentation-hint-table* (symbol-value tables))))
        (setf *indentation-hint-table* table))
      t)))

(hint-slime-indentation)
