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

(defpackage :esrap
  (:use :cl :alexandria)
  #+sbcl
  (:lock t)
  (:export
   #:&bounds

   #:! #:? #:+ #:* #:& #:~

   #:add-rule
   #:change-rule
   #:concat
   #:defrule
   #:describe-grammar
   #:esrap-error
   #:esrap-error-position
   #:esrap-error-text
   #:find-rule
   #:left-recursion
   #:left-recursion-nonterminal
   #:left-recursion-path
   #:make-grammar
   #:parse
   #:remove-rule
   #:rule
   #:rule-dependencies
   #:rule-expression
   #:rule-symbol
   #:text
   #:trace-rule
   #:untrace-rule
   ))

(in-package :esrap)

;;; Conditions

(define-condition esrap-error (parse-error)
  ((text :initarg :text :initform nil :reader esrap-error-text)
   (position :initarg :position :initform nil :reader esrap-error-position))
  (:documentation
   "Signaled when an Esrap parse fails. Use ESRAP-ERROR-TEXT to obtain the
string that was being parsed, and ESRAP-ERROR-POSITION the position at which
the error occurred."))

(defmethod print-object ((condition esrap-error) stream)
  (if *print-escape*
      (call-next-method)
      ;; FIXME: this looks like it won't do the right thing when used as part of a
      ;; logical block.
      (when (or (not *print-lines*) (> *print-lines* 1))
        (if-let ((text (esrap-error-text condition))
                 (position (esrap-error-position condition)))
                (let* ((line (count #\Newline text :end position))
                       (column (- position (or (position #\Newline text
                                                         :end position
                                                         :from-end t)
                                               0)
                                  1))
                       ;; FIXME: magic numbers
                       (start (or (position #\Newline text
                                            :start (max 0 (- position 32))
                                            :end (max 0 (- position 24))
                                            :from-end t)
                                  (max 0 (- position 24))))
                       (end (min (length text) (+ position 24)))
                       (newline (or (position #\Newline text
                                              :start start
                                              :end position
                                              :from-end t)
                                    start))
                       (*print-circle* nil))
                  (format stream "~2&  Encountered at:~%    ~
                                  ~A~%    ~
                                  ~V@T^ (Line ~D, Column ~D, Position ~D)~%"
                          (if (emptyp text)
                              ""
                              (subseq text start end))
                          (- position newline)
                          (1+ line) (1+ column)
                          position))
                (format stream "~2&  <text and position not available>")))))

(define-condition simple-esrap-error (esrap-error simple-condition) ())

(defmethod print-object :before ((condition simple-esrap-error) stream)
  (apply #'format stream
         (simple-condition-format-control condition)
         (simple-condition-format-arguments condition)))

(declaim (ftype (function (t t t &rest t) (values nil &optional))
                simple-esrap-error))
(defun simple-esrap-error (text position format-control &rest format-arguments)
  (error 'simple-esrap-error
         :text text
         :position position
         :format-control format-control
         :format-arguments format-arguments))

(define-condition left-recursion (esrap-error)
  ((nonterminal :initarg :nonterminal :initform nil :reader left-recursion-nonterminal)
   (path :initarg :path :initform nil :reader left-recursion-path))
  (:documentation
   "Signaled when left recursion is detected during Esrap parsing.
LEFT-RECURSION-NONTERMINAL names the symbol for which left recursion was
detected, and LEFT-RECURSION-PATH lists nonterminals of which the left
recursion cycle consists."))

(defmethod print-object :before ((condition left-recursion) stream)
  (format stream "Left recursion in nonterminal ~S. ~_Path: ~
                  ~{~S~^ -> ~}"
          (left-recursion-nonterminal condition)
          (left-recursion-path condition)))

;;; Miscellany

(defun text (&rest arguments)
  "Arguments must be strings, or lists whose leaves are strings.
Catenates all the strings in arguments into a single string."
  (with-output-to-string (s)
    (labels ((cat-list (list)
               (dolist (elt list)
                 (etypecase elt
                   (string (write-string elt s))
                   (character (write-char elt s))
                   (list (cat-list elt))))))
      (cat-list arguments))))

(setf (symbol-function 'concat) (symbol-function 'text))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun note-deprecated (old new)
    (warn 'simple-style-warning
          :format-control "~S is deprecated, use ~S instead."
          :format-arguments (list old new))))

(define-compiler-macro concat (&whole form &rest arguments)
  (declare (ignore arguments))
  (note-deprecated 'concat 'text)
  form)

(defun text/bounds (strings start end)
  (declare (ignore start end))
  (text strings))

(defun lambda/bounds (function)
  (lambda (result start end)
    (declare (ignore start end))
    (funcall function result)))

(defun identity/bounds (identity start end)
  (declare (ignore start end))
  identity)

(defun parse-lambda-list-maybe-containing-&bounds (lambda-list)
  "Parse &BOUNDS section in LAMBDA-LIST and return three values:

1. The standard lambda list sublist of LAMBDA-LIST
2. A symbol that should be bound to the start of a matching substring
3. A symbol that should be bound to the end of a matching substring
4. A list containing symbols that were GENSYM'ed.

The second and/or third values are GENSYMS if LAMBDA-LIST contains a
partial or no &BOUNDS section, in which case fourth value contains them
for use with IGNORE."
  (let ((length (length lambda-list)))
    (multiple-value-bind (lambda-list start end gensyms)
        (cond
          ;; Look for &BOUNDS START END.
          ((and (>= length 3)
                (eq (nth (- length 3) lambda-list) '&bounds))
           (values (subseq lambda-list 0 (- length 3))
                   (nth (- length 2) lambda-list)
                   (nth (- length 1) lambda-list)
                   nil))
          ;; Look for &BOUNDS START.
          ((and (>= length 2)
                (eq (nth (- length 2) lambda-list) '&bounds))
           (let ((end (gensym "END")))
             (values (subseq lambda-list 0 (- length 2))
                     (nth (- length 1) lambda-list)
                     end
                     (list end))))
          ;; No &BOUNDS section.
          (t
           (let ((start (gensym "START"))
                 (end (gensym "END")))
             (values lambda-list
                     start
                     end
                     (list start end)))))
      (check-type start symbol)
      (check-type end symbol)
      (values lambda-list start end gensyms))))

(deftype nonterminal ()
  "Any symbol except CHARACTER and NIL can be used as a nonterminal symbol."
  '(and symbol (not (member character nil))))

(deftype terminal ()
  "Literal strings and characters are used as case-sensitive terminal symbols,
and expressions of the form \(~ <literal>) denote case-insensitive terminals."
  `(or string character
       (cons (eql ~) (cons (or string character) null))))

;;; RULE REPRESENTATION AND STORAGE
;;;
;;; For each rule, there is a RULE-CELL in GRAMMAR-RULES, whose %INFO slot has the
;;; function that implements the rule in car, and the rule object in CDR. A
;;; RULE object can be attached to only one non-terminal at a time, which is
;;; accessible via RULE-SYMBOL.

(defclass grammar ()
    ((rules :initform (make-hash-table) :accessor grammar-rules)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-name (suffix &optional name)
    (intern (string-upcase (concatenate 'string
					(when name (princ-to-string name))
					(when name "-") suffix)))))

(defmacro make-grammar (name)
  `(%make-grammar ,name))

(defmacro %make-grammar (&optional name)
  (let ((grammar (gensym))
	(name-defrule (make-name "defrule" name))
	(name-parse (make-name "parse" name))
	(name-describe-grammar (make-name "describe-grammar" name))
	(name-trace-rule (make-name "trace-rule" name))
	(name-untrace-rule (make-name "untrace-rule" name))
	(name-find-rule (make-name "find-rule" name))
	(name-change-rule (make-name "change-rule" name))
	(name-add-rule (make-name "add-rule" name))
	(name-remove-rule (make-name "remove-rule" name))
	(name-rule-dependencies (make-name "rule-dependencies" name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((,grammar (make-instance 'grammar)))
	 (defmacro ,name-defrule (&whole form symbol expression &body options)
	   ,(documentation '%defrule 'function)
	   `(%defrule ,',name-add-rule ,form ,symbol ,expression ,@options))
	 (defun ,name-parse (expression text &key (start 0) end junk-allowed)
	   ,(documentation '%parse 'function)
	   (%parse ,grammar expression text
		   :start start
		   :end end
		   :junk-allowed junk-allowed))
	 (defun ,name-describe-grammar (symbol
					&optional (stream *standard-output*))
	   ,(documentation '%describe-grammar 'function)
	   (%describe-grammar ,grammar symbol stream))
	 (defun ,name-trace-rule (symbol &key recursive break)
	   ,(documentation '%trace-rule 'function)
	   (%trace-rule ,grammar symbol :recursive recursive :break break))
	 (defun ,name-untrace-rule (symbol &key recursive break)
	   ,(documentation '%untrace-rule 'function)
	   (%untrace-rule ,grammar symbol :recursive recursive :break break))
	 (defun ,name-find-rule (symbol)
	   ,(documentation '%find-rule 'function)
	   (%find-rule ,grammar symbol))
	 (defun ,name-change-rule (symbol expression)
	   ,(documentation '%change-rule 'function)
	   (%change-rule ,grammar symbol expression))
	 (defun ,name-add-rule (symbol rule)
	   ,(documentation '%add-rule 'function)
	   (%add-rule ,grammar symbol rule))
	 (defun ,name-remove-rule (symbol &key (force t))
	   ,(documentation '%remove-rule 'function)
	   (%remove-rule ,grammar symbol :force force))
	 (defun ,name-rule-dependencies (rule)
	   ,(documentation '%rule-dependencies 'function)
	   (%rule-dependencies ,grammar rule))))))

(defun clear-rules (grammar)
  (clrhash (grammar-rules grammar))
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

(defun ensure-rule-cell (grammar symbol)
  (check-type symbol nonterminal)
  ;; FIXME: Need to lock GRAMMAR-RULES.
  (or (gethash symbol (grammar-rules grammar))
      (setf (gethash symbol (grammar-rules grammar))
            (make-rule-cell symbol))))

(defun delete-rule-cell (grammar symbol)
  (remhash symbol (grammar-rules grammar)))

(defun reference-rule-cell (grammar symbol referent)
  (let ((cell (ensure-rule-cell grammar symbol)))
    (when referent
      (pushnew referent (cell-referents cell)))
    cell))

(defun dereference-rule-cell (grammar symbol referent)
  (let ((cell (ensure-rule-cell grammar symbol)))
    (setf (cell-referents cell) (delete referent (cell-referents cell)))
    cell))

(defun find-rule-cell (grammar symbol)
  (check-type symbol nonterminal)
  (gethash symbol (grammar-rules grammar)))

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
    :initform nil
    :reader rule-condition)
   (%transform
    :initarg :transform
    :initform nil
    :reader rule-transform)))

(defun rule-symbol (rule)
  "Returns the nonterminal associated with the RULE, or NIL of the rule
is not attached to any nonterminal."
  (slot-value rule '%symbol))

(defun detach-rule (grammar rule)
  (dolist (dep (%rule-direct-dependencies rule))
    (dereference-rule-cell grammar dep (rule-symbol rule)))
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

(defun sort-dependencies (grammar symbol dependencies)
  (let ((symbols (delete symbol dependencies))
        (defined nil)
        (undefined nil))
    (dolist (sym symbols)
      (if (%find-rule grammar sym)
          (push sym defined)
          (push sym undefined)))
    (values defined undefined)))

(defun %rule-dependencies (grammar rule)
  "Returns the dependencies of the RULE: primary value is a list of defined
nonterminal symbols, and secondary value is a list of undefined nonterminal
symbols."
  (sort-dependencies
   grammar
   (rule-symbol rule) (%expression-dependencies grammar (rule-expression rule) nil)))

(defun rule-direct-dependencies (grammar rule)
  (sort-dependencies
   grammar
   (rule-symbol rule) (%expression-direct-dependencies (rule-expression rule) nil)))

(defun %rule-direct-dependencies (rule)
  (delete (rule-symbol rule) (%expression-direct-dependencies (rule-expression rule) nil)))

;;; Expression destructuring and validation

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

;;; MEMOIZATION CACHE
;;;
;;; Because each [rule, position] tuple has an unambiguous
;;; result per source text, we can cache this result -- this is what
;;; makes packrat parsing O(N).
;;;
;;; For now we just use EQUAL hash-tables, but a specialized
;;; representation would probably pay off.

(defvar *cache*)

(defun make-cache ()
  (make-hash-table :test #'equal))

(defun get-cached (symbol position cache)
  (gethash (cons symbol position) cache))

(defun (setf get-cached) (result symbol position cache)
  (setf (gethash (cons symbol position) cache) result))

(defvar *nonterminal-stack* nil)

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

(defun %parse (grammar expression text &key (start 0) end junk-allowed)
  "Parses TEXT using EXPRESSION from START to END. Incomplete parses
are allowed only if JUNK-ALLOWED is true."
  ;; There is no backtracking in the toplevel expression -- so there's
  ;; no point in compiling it as it will be executed only once -- unless
  ;; it's a constant, for which we have a compiler-macro.
  (let ((end (or end (length text))))
    (process-parse-result
     (let ((*cache* (make-cache)))
       (eval-expression grammar expression text start end))
     text
     end
     junk-allowed)))

(define-compiler-macro %parse (&whole form grammar expression &rest arguments
				      &environment env)
  (if (constantp expression env)
      (with-gensyms (expr-fun)
        `(let ((,expr-fun (load-time-value (compile-expression ,grammar
							       ,expression))))
           ;; This inline-lambda here provides keyword defaults and
           ;; parsing, so the compiler-macro doesn't have to worry
           ;; about evaluation order.
           ((lambda (text &key (start 0) end junk-allowed)
              (let ((*cache* (make-cache))
                    (end (or end (length text))))
                (process-parse-result
                 (funcall ,expr-fun text start end)
                 text
                 end
                 junk-allowed)))
            ,@arguments)))
      form))

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

(defmacro %defrule (name-add-rule form symbol expression &body options)
  "Define SYMBOL as a nonterminal, using EXPRESSION as associated the parsing expression.

Following OPTIONS can be specified:

  * (:WHEN TEST)

    The rule is active only when TEST evaluates to true. This can be used
    to specify optional extensions to a grammar.

  * (:CONSTANT CONSTANT)

    No matter what input is consumed or what EXPRESSION produces, the production
    of the rule is always CONSTANT.

  * (:FUNCTION FUNCTION)

    If provided the production of the expression is transformed using
    FUNCTION. FUNCTION can be a function name or a lambda-expression.

  * (:IDENTITY BOOLEAN)

    If true, the production of expression is used as-is, as if (:FUNCTION IDENTITY)
    has been specified. If no production option is specified, this is the default.

  * (:TEXT BOOLEAN)

    If true, the production of expression is flattened and concatenated into a string
    as if by (:FUNCTION TEXT) has been specified.

  * (:LAMBDA LAMBDA-LIST &BODY BODY)

    If provided, same as using the corresponding lambda-expression with :FUNCTION.

    As an extension of the standard lambda list syntax, LAMBDA-LIST accepts
    the optional pseudo lambda-list keyword ESRAP:&BOUNDS, which (1) must appear
    after all standard lambda list keywords. (2) can be followed by one or two
    variables to which bounding indexes of the matching substring are bound.

    Therefore:

      LAMBDA-LIST ::= (STANDARD-LAMBDA-LIST-ELEMENTS [&BOUNDS START [END]])

  * (:DESTRUCTURE DESTRUCTURING-LAMBDA-LIST &BODY BODY)

    If provided, same as using a lambda-expression that destructures its argument
    using DESTRUCTURING-BIND and the provided lambda-list with :FUNCTION.

    DESTRUCTURING-LAMBDA-LIST can use ESRAP:&BOUNDS in the same way
    as described for :LAMBDA.
"
  (let ((transform nil)
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
          (ecase (car option)
            ((:when)
             (let ((expr (second option)))
               (when (cddr option)
                 (error "Multiple expressions in a :WHEN:~% ~S" form))
               (if (constantp expr)
                   (if (eval expr)
                       (set-guard expr t)
                       (set-guard expr nil))
                   (set-guard expr `(lambda () ,expr)))))
            ((:constant)
             (setf transform `(constantly ,(second option))))
            ((:concat)
             (note-deprecated :concat :text)
             (when (second option)
               (setf transform '#'text/bounds)))
            ((:text)
             (when (second option)
               (setf transform '#'text/bounds)))
            ((:identity)
             (when (second option)
               (setf transform '#'identity/bounds)))
            ((:lambda)
             (destructuring-bind (lambda-list &body forms) (cdr option)
               (multiple-value-bind (lambda-list start end ignore)
                   (parse-lambda-list-maybe-containing-&bounds lambda-list)
                 (setf transform
                       `(lambda (,@lambda-list ,start ,end)
                          (declare (ignore ,@ignore))
                          ,@forms)))))
            ((:function)
             (setf transform `(lambda/bounds (function ,(second option)))))
            ((:destructure)
             (destructuring-bind (lambda-list &body forms) (cdr option)
               (multiple-value-bind (lambda-list start end ignore)
                   (parse-lambda-list-maybe-containing-&bounds lambda-list)
                 (setf transform
                       (with-gensyms (production)
                         `(lambda (,production ,start ,end)
                            (declare (ignore ,@ignore))
                            (destructuring-bind ,lambda-list ,production
                              ,@forms)))))))))))
    `(eval-when (:load-toplevel :execute)
       (,name-add-rule ',symbol
		       (make-instance 'rule
				      :expression ',expression
				      :guard-expression ',guard
				      :transform ,(or transform '#'identity/bounds)
				      :condition ,condition)))))

(defun %add-rule (grammar symbol rule)
  "Associates RULE with the nonterminal SYMBOL. Signals an error if the
rule is already associated with a nonterminal. If the symbol is already
associated with a rule, the old rule is removed first."
  ;; FIXME: This needs locking and WITHOUT-INTERRUPTS.
  (check-type symbol nonterminal)
  (when (rule-symbol rule)
    (error "~S is already associated with the nonterminal ~S -- remove it first."
           rule (rule-symbol rule)))
  (let* ((cell (ensure-rule-cell grammar symbol))
         (function (compile-rule grammar
				 symbol
                                 (rule-expression rule)
                                 (rule-condition rule)
                                 (rule-transform rule)))
         (trace-info (cell-trace-info cell)))
    (set-cell-info cell function rule)
    (setf (cell-trace-info cell) nil)
    (setf (slot-value rule '%symbol) symbol)
    (when trace-info
      (%trace-rule grammar symbol :break (second trace-info)))
    symbol))

(defun %find-rule (grammar symbol)
  "Returns rule designated by SYMBOL, if any. Symbol must be a nonterminal
symbol."
  (check-type symbol nonterminal)
  (let ((cell (find-rule-cell grammar symbol)))
    (when cell
      (cell-rule cell))))

(defun %remove-rule (grammar symbol &key force)
  "Makes the nonterminal SYMBOL undefined. If the nonterminal is defined an
already referred to by other rules, an error is signalled unless :FORCE is
true."
  (check-type symbol nonterminal)
  ;; FIXME: Lock and WITHOUT-INTERRUPTS.
  (let* ((cell (find-rule-cell grammar symbol))
         (rule (cell-rule cell))
         (trace-info (cell-trace-info cell)))
    (when cell
      (flet ((frob ()
               (set-cell-info cell (undefined-rule-function symbol) nil)
               (when trace-info
                 (setf (cell-trace-info cell) (list (cell-%info cell) (second trace-info))))
               (when rule
                 (detach-rule grammar rule))))
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
                 (delete-rule-cell grammar symbol)))))
      rule)))

(defvar *trace-level* 0)

(defvar *trace-stack* nil)

(defun %trace-rule (grammar symbol &key recursive break)
  "Turn on tracing of nonterminal SYMBOL. If RECURSIVE is true, turn
on tracing for the whole grammar rooted at SYMBOL. If BREAK is true,
break is entered when the rule is invoked."
  (unless (member symbol *trace-stack* :test #'eq)
    (let ((cell (find-rule-cell grammar symbol)))
      (unless cell
        (error "Undefined rule: ~S" symbol))
      (when (cell-trace-info cell)
        (let ((*trace-stack* nil))
          (%untrace-rule grammar symbol)))
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
            (%trace-rule grammar dep :recursive t :break break))))
      t)))

(defun %untrace-rule (grammar symbol &key recursive break)
  "Turn off tracing of nonterminal SYMBOL. If RECURSIVE is true, untraces the
whole grammar rooted at SYMBOL. BREAK is ignored, and is provided only for
symmetry with TRACE-RULE."
  (declare (ignore break))
  (unless (member symbol *trace-stack* :test #'eq)
    (let ((cell (find-rule-cell grammar symbol)))
      (unless cell
        (error "Undefined rule: ~S" symbol))
      (let ((trace-info (cell-trace-info cell)))
        (when trace-info
          (setf (cell-%info cell) (car trace-info)
                (cell-trace-info cell) nil))
        (when recursive
          (let ((*trace-stack* (cons symbol *trace-stack*)))
            (dolist (dep (%rule-direct-dependencies (cell-rule cell)))
              (%untrace-rule grammar dep :recursive t))))))
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

(defun %change-rule (grammar symbol expression)
  "Modifies the nonterminal SYMBOL to use EXPRESSION instead. Temporarily
removes the rule while it is being modified."
  (let ((rule (%remove-rule grammar symbol :force t)))
    (unless rule
      (error "~S is not a defined rule." symbol))
    (setf (rule-expression rule) expression)
    (%add-rule grammar symbol rule)))

(defun symbol-length (x)
  (length (symbol-name x)))

(defun %describe-grammar (grammar symbol &optional (stream *standard-output*))
  "Prints the grammar tree rooted at nonterminal SYMBOL to STREAM for human
inspection."
  (check-type symbol nonterminal)
  (let ((rule (%find-rule grammar symbol)))
    (cond ((not rule)
           (format stream "Symbol ~S is not a defined nonterminal." symbol))
          (t
           (format stream "~&Grammar ~S:~%" symbol)
           (multiple-value-bind (defined undefined) (%rule-dependencies grammar rule)
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
                   (let ((dep (%find-rule grammar s)))
                     (format stream "~3T~S~VT<- ~S~@[ : ~S~]~%"
                            s length (rule-expression dep)
                            (when (rule-condition rule)
                              (rule-guard-expression rule))))))
               (when undefined
                 (format stream "~%Undefined nonterminal~P:~%~{~3T~S~%~}"
                         (length undefined) undefined))))))))

;;; COMPILING RULES

(defvar *current-rule* nil)

(defun compile-rule (grammar symbol expression condition transform)
  (declare (type (or boolean function) condition transform))
  (let* ((*current-rule* symbol)
         ;; Must bind *CURRENT-RULE* before compiling the expression!
         (function (compile-expression grammar expression))
         (rule-not-active (when condition (make-inactive-rule :name symbol))))
    (cond ((not condition)
           (named-lambda inactive-rule (text position end)
             (declare (ignore text position end))
             rule-not-active))
          (transform
           (flet ((exec-rule/transform (text position end)
                    (let ((result (funcall function text position end)))
                      (if (error-result-p result)
                          (make-failed-parse
                           :expression symbol
                           :position (if (failed-parse-p result)
                                         (failed-parse-position result)
                                         position)
                           :detail result)
                          (make-result
                           :position (result-position result)
                           :production (funcall transform
                                                (result-production result)
                                                position
                                                (result-position result)))))))
             (if (eq t condition)
                 (named-lambda rule/transform (text position end)
                   (with-cached-result (symbol position text)
                     (exec-rule/transform text position end)))
                 (named-lambda condition-rule/transform (text position end)
                   (with-cached-result (symbol position text)
                     (if (funcall condition)
                         (exec-rule/transform text position end)
                         rule-not-active))))))
          (t
           (if (eq t condition)
               (named-lambda rule (text position end)
                 (with-cached-result (symbol position text)
                   (funcall function text position end)))
               (named-lambda conditional-rule (text position end)
                 (with-cached-result (symbol position text)
                   (if (funcall condition)
                       (funcall function text position end)
                       rule-not-active))))))))

;;; EXPRESSION COMPILER & EVALUATOR

(defun invalid-expression-error (expression)
  (error "Invalid expression: ~S" expression))

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
           ((and or)
            (and (every #'validate-expression (cdr expression)) t))
           ((nil)
            nil)
           (string
            (and (cdr expression) (not (cddr expression))
                 (typep (second expression) 'array-length)))
           (t
            (and (symbolp (car expression))
                 (cdr expression) (not (cddr expression))
                 (validate-expression (second expression))))))
        (t
         nil))
      (invalid-expression-error expression)))

(defun %expression-dependencies (grammar expression seen)
  (etypecase expression
    ((member character)
     seen)
    (terminal
     seen)
    (nonterminal
     (if (member expression seen :test #'eq)
         seen
         (let ((rule (%find-rule grammar expression))
               (seen (cons expression seen)))
           (if rule
               (%expression-dependencies grammar (rule-expression rule) seen)
               seen))))
    (cons
     (case (car expression)
       (string
        seen)
       ((and or)
        (dolist (subexpr (cdr expression) seen)
          (setf seen (%expression-dependencies grammar subexpr seen))))
       ((* + ? & !)
        (%expression-dependencies grammar (second expression) seen))
       (t
        (%expression-dependencies grammar (second expression) seen))))))

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

(defun eval-expression (grammar expression text position end)
  (typecase expression
    ((eql character)
     (eval-character text position end))
    (terminal
     (if (consp expression)
         (eval-terminal (string (second expression)) text position end nil)
         (eval-terminal (string expression) text position end t)))
    (nonterminal
     (eval-nonterminal grammar expression text position end))
    (cons
     (case (car expression)
       (string
        (eval-string expression text position end))
       (and
        (eval-sequence grammar expression text position end))
       (or
        (eval-ordered-choise grammar expression text position end))
       (*
        (eval-greedy-repetition grammar expression text position end))
       (+
        (eval-greedy-positive-repetition grammar expression text position end))
       (?
        (eval-optional grammar expression text position end))
       (&
        (eval-followed-by grammar expression text position end))
       (!
        (eval-not-followed-by grammar expression text position end))
       (t
        (if (symbolp (car expression))
            (eval-semantic-predicate grammar expression text position end)
            (invalid-expression-error expression)))))
    (t
     (invalid-expression-error expression))))

(defun compile-expression (grammar expression)
  (etypecase expression
    ((eql character)
     (compile-character))
    (terminal
     (if (consp expression)
         (compile-terminal (string (second expression)) nil)
         (compile-terminal (string expression) t)))
    (nonterminal
     (compile-nonterminal grammar expression))
    (cons
     (case (car expression)
       (string
        (compile-string expression))
       (and
        (compile-sequence grammar expression))
       (or
        (compile-ordered-choise grammar expression))
       (*
        (compile-greedy-repetition grammar expression))
       (+
        (compile-greedy-positive-repetition grammar expression))
       (?
        (compile-optional grammar expression))
       (&
        (compile-followed-by grammar expression))
       (!
        (compile-not-followed-by grammar expression))
       (t
        (if (symbolp (car expression))
            (compile-semantic-predicate grammar expression)
            (invalid-expression-error expression)))))
    (t
     (invalid-expression-error expression))))

;;; Characters and strings

(declaim (inline exec-string))
(defun exec-string (length text position end)
  (let ((limit (+ length position)))
    (if (<= limit end)
        (make-result
         :production (subseq text position limit)
         :position limit)
        (make-failed-parse
         :expression `(string ,length)
         :position position))))

(defun eval-character (text position end)
  (if (< position end)
      (make-result
       :production (char text position)
       :position (1+ position))
      (make-failed-parse
       :expression 'character
       :position position)))

(defun compile-character ()
  #'eval-character)

(defun eval-string (expression text position end)
  (with-expression (expression (string length))
    (exec-string length text position end)))

(defun compile-string (expression)
  (with-expression (expression (string length))
    (named-lambda compiled-string (text position end)
      (exec-string length text position end))))

;;; Terminals
;;;
;;; FIXME: It might be worth it to special-case terminals of length 1.

(declaim (inline match-terminal-p))
(defun match-terminal-p (string length text position end case-sensitive-p)
  (and (<= (+ length position) end)
       (if case-sensitive-p
           (string= string text :start2 position :end2 (+ position length))
           (string-equal string text :start2 position :end2 (+ position length)))))

(defun exec-terminal (string length text position end case-sensitive-p)
  (if (match-terminal-p string length text position end case-sensitive-p)
      (make-result
       :position (+ length position)
       :production string)
      (make-failed-parse
       :expression string
       :position position)))

(defun eval-terminal (string text position end case-sensitive-p)
  (exec-terminal string (length string) text position end case-sensitive-p))

(defun compile-terminal (string case-sensitive-p)
  (let ((length (length string)))
    (named-lambda compiled-terminal (text position end)
      (exec-terminal string length text position end case-sensitive-p))))

;;; Nonterminals

(defparameter *eval-nonterminals* nil)

(defun eval-nonterminal (grammar symbol text position end)
  (if *eval-nonterminals*
      (eval-expression grammar (rule-expression (%find-rule grammar symbol)) text position end)
      (funcall (cell-function (ensure-rule-cell grammar symbol)) text position end)))

(defun compile-nonterminal (grammar symbol)
  (let ((cell (reference-rule-cell grammar symbol *current-rule*)))
    (declare (rule-cell cell))
    (named-lambda compile-nonterminal (text position end)
      (funcall (cell-function cell) text position end))))

;;; Sequences
;;;
;;; FIXME: It might be better if we actually chained the closures
;;; here, instead of looping over them -- benchmark first, though.

(defun eval-sequence (grammar expression text position end)
  (with-expression (expression (and &rest subexprs))
    (let (results)
      (dolist (expr subexprs
               (make-result
                :position position
                :production (mapcar #'result-production (nreverse results))))
        (let ((result (eval-expression grammar expr text position end)))
          (if (error-result-p result)
              (return (make-failed-parse
                       :expression expression
                       :position position
                       :detail result))
              (setf position (result-position result)))
          (push result results))))))

(defun compile-sequence (grammar expression)
  (with-expression (expression (and &rest subexprs))
    (let ((functions (mapcar (lambda (subexpr)
			       (compile-expression grammar subexpr))
			     subexprs)))
      (named-lambda compiled-sequence (text position end)
          (let (results)
            (dolist (fun functions
                     (make-result
                      :position position
                      :production (mapcar #'result-production (nreverse results))))
              (let ((result (funcall fun text position end)))
                (if (error-result-p result)
                    (return (make-failed-parse
                             :expression expression
                             :position position
                             :detail result))
                    (setf position (result-position result)))
                (push result results))))))))

;;; Ordered choises

(defun eval-ordered-choise (grammar expression text position end)
  (with-expression (expression (or &rest subexprs))
    (let (last-error)
      (dolist (expr subexprs
               (make-failed-parse
                :expression expression
                :position (if (failed-parse-p last-error)
                              (failed-parse-position last-error)
                              position)
                :detail last-error))
        (let ((result (eval-expression grammar expr text position end)))
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

(defun compile-ordered-choise (grammar expression)
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
         ;; choise with a single string.
         (let ((choises (apply #'concatenate 'string canonized)))
           (named-lambda compiled-character-choise (text position end)
             (let ((c (and (< position end) (find (char text position) choises))))
               (if c
                   (make-result :position (+ 1 position)
                                :production (string c))
                   (make-failed-parse
                    :expression expression
                    :position position))))))
        (:strings
         ;; If every subexpression is a string, we can represent the whole choise
         ;; with a list of strings.
         (let ((choises (nreverse canonized)))
           (named-lambda compiled-character-choise (text position end)
             (dolist (choise choises
                      (make-failed-parse
                       :expression expression
                       :position position))
               (let ((len (length choise)))
                 (when (match-terminal-p choise len text position end t)
                   (return
                     (make-result :position (+ len position)
                                  :production choise))))))))
        (:general
         ;; In the general case, compile subexpressions and call.
         (let ((functions (mapcar (lambda (subexpr)
				    (compile-expression grammar subexpr))
				  subexprs)))
             (named-lambda compiled-ordered-choise (text position end)
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

;;; Greedy repetitions

(defun eval-greedy-repetition (grammar expression text position end)
  (funcall (compile-greedy-repetition grammar expression) text position end))

(defun compile-greedy-repetition (grammar expression)
  (with-expression (expression (* subexpr))
    (let ((function (compile-expression grammar subexpr)))
      (named-lambda compiled-greedy-repetition (text position end)
        (let ((results
               (loop for result = (funcall function text position end)
                     until (error-result-p result)
                     do (setf position (result-position result))
                     collect result)))
          (make-result
           :position position
           :production (mapcar #'result-production results)))))))

;;; Greedy positive repetitions

(defun eval-greedy-positive-repetition (grammar expression text position end)
  (funcall (compile-greedy-positive-repetition grammar expression)
           text position end))

(defun compile-greedy-positive-repetition (grammar expression)
  (with-expression (expression (+ subexpr))
    (let ((function (compile-expression grammar subexpr)))
      (named-lambda compiled-greedy-positive-repetition (text position end)
        (let* ((last nil)
               (results
                (loop for result = (funcall function text position end)
                     until (error-result-p (setf last result))
                     do (setf position (result-position result))
                     collect result)))
          (if results
              (make-result
               :position position
               :production (mapcar #'result-production results))
              (make-failed-parse
               :position position
               :expression expression
               :detail last)))))))

;;; Optionals

(defun eval-optional (grammar expression text position end)
  (with-expression (expression (? subexpr))
    (let ((result (eval-expression grammar subexpr text position end)))
      (if (error-result-p result)
          (make-result :position position)
          result))))

(defun compile-optional (grammar expression)
  (with-expression (expression (? subexpr))
    (let ((function (compile-expression grammar subexpr)))
      (named-lambda compiled-optional (text position end)
        (let ((result (funcall function text position end)))
          (if (error-result-p result)
              (make-result :position position)
              result))))))

;;; Followed-by's

(defun eval-followed-by (grammar expression text position end)
  (with-expression (expression (& subexpr))
    (let ((result (eval-expression grammar subexpr text position end)))
      (if (error-result-p result)
          (make-failed-parse
           :position position
           :expression expression
           :detail result)
          (make-result
           :position position
           :production (result-production result))))))

(defun compile-followed-by (grammar expression)
  (with-expression (expression (& subexpr))
    (let ((function (compile-expression grammar subexpr)))
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

;;; Not followed-by's

(defun eval-not-followed-by (grammar expression text position end)
  (with-expression (expression (! subexpr))
    (let ((result (eval-expression grammar subexpr text position end)))
      (if (error-result-p result)
          (make-result
           :position position)
          (make-failed-parse
           :expression expression
           :position position)))))

(defun compile-not-followed-by (grammar expression)
  (with-expression (expression (! subexpr))
    (let ((function (compile-expression grammar subexpr)))
      (named-lambda compiled-not-followed-by (text position end)
        (let ((result (funcall function text position end)))
          (if (error-result-p result)
              (make-result
               :position position)
              (make-failed-parse
               :expression expression
               :position position)))))))

;;; Semantic predicates

(defun eval-semantic-predicate (grammar expression text position end)
  (with-expression (expression (t subexpr))
    (let ((result (eval-expression grammar subexpr text position end)))
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

(defun compile-semantic-predicate (grammar expression)
  (with-expression (expression (t subexpr))
    (let* ((function (compile-expression grammar subexpr))
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

(%make-grammar)