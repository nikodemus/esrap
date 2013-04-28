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
  (:use :cl :alexandria :defmacro-enhance :iterate)
  (:shadowing-import-from :rutils.string :strcat)
  #+sbcl
  (:lock t)
  (:export
   #:&bounds

   #:context
   #:! #:? #:+ #:* #:& #:~ #:<- #:->
   #:character-ranges #:wrap

   #:add-rule
   #:call-transform
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

(defun foo () nil)

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
;;; For each rule, there is a RULE-CELL in *RULES*, whose %INFO slot has the
;;; function that implements the rule in car, and the rule object in CDR. A
;;; RULE object can be attached to only one non-terminal at a time, which is
;;; accessible via RULE-SYMBOL.

(defvar *rules* (make-hash-table))

(defun clear-rules ()
  (clrhash *rules*)
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

(defun ensure-rule-cell (symbol)
  (check-type symbol nonterminal)
  ;; FIXME: Need to lock *RULES*.
  (or (gethash symbol *rules*)
      (setf (gethash symbol *rules*)
            (make-rule-cell symbol))))

(defun delete-rule-cell (symbol)
  (remhash symbol *rules*))

(defun reference-rule-cell (symbol referent)
  (let ((cell (ensure-rule-cell symbol)))
    (when referent
      (pushnew referent (cell-referents cell)))
    cell))

(defun dereference-rule-cell (symbol referent)
  (let ((cell (ensure-rule-cell symbol)))
    (setf (cell-referents cell) (delete referent (cell-referents cell)))
    cell))

(defun find-rule-cell (symbol)
  (check-type symbol nonterminal)
  (gethash symbol *rules*))

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
    :initform t
    :reader rule-condition)
   (%transform
    :initarg :transform
    :initform nil
    :reader rule-transform)
   (%around
    :initarg :around
    :initform nil
    :reader rule-around)))

(defun rule-symbol (rule)
  "Returns the nonterminal associated with the RULE, or NIL of the rule
is not attached to any nonterminal."
  (slot-value rule '%symbol))

(defun detach-rule (rule)
  (dolist (dep (%rule-direct-dependencies rule))
    (dereference-rule-cell dep (rule-symbol rule)))
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

(defun sort-dependencies (symbol dependencies)
  (let ((symbols (delete symbol dependencies))
        (defined nil)
        (undefined nil))
    (dolist (sym symbols)
      (if (find-rule sym)
          (push sym defined)
          (push sym undefined)))
    (values defined undefined)))

(defun rule-dependencies (rule)
  "Returns the dependencies of the RULE: primary value is a list of defined
nonterminal symbols, and secondary value is a list of undefined nonterminal
symbols."
  (sort-dependencies
   (rule-symbol rule) (%expression-dependencies (rule-expression rule) nil)))

(defun rule-direct-dependencies (rule)
  (sort-dependencies
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

(defparameter context :void "Context, which is active, when the rule is trying to parse.
Cache depends not only on rule-name and position, but also on the context assumed while
parsing.")

(defvar *cache*)

(defun make-cache ()
  (make-hash-table :test #'equal))

(defun get-cached (symbol position cache)
  (gethash (list symbol position context) cache))

(defun (setf get-cached) (result symbol position cache)
  (setf (gethash (list symbol position context) cache) result))

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

(defmacro! defrule (&whole form symbol expression &body options)
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

  * (:AROUND ([&BOUNDS START [END]]) &BODY BODY)

    If provided, execute BODY around the construction of the production of the
    rule. BODY has to call ESRAP:CALL-TRANSFORM to trigger the computation of
    the production. Any transformation provided via :LAMBDA, :FUNCTION
    or :DESTRUCTURE is executed inside the call to ESRAP:CALL-TRANSFORM. As a
    result, modification to the dynamic state are visible within the
    transform.

    ESRAP:&BOUNDS can be used in the same way as described for :LAMBDA
    and :DESTRUCTURE.

    This option can be used to safely track nesting depth, manage symbol
    tables or for other stack-like operations.
  * (:WRAP-AROUND &BODY BODY)

    Another way to perform stack-like operations.
    Shadows everything, that's specified in the :AROUND clause.
    If used, it is assumed, that EXPRESSION is of the form (LIST 'WRAP WRAPPER WRAPPIE),
    where WRAPPER and WRAPPIE are arbitrary expressions, not containing WRAP.
    All this being the case, parsing of a rule proceeds as follows:
       * first, WRAPPER is parsed.
       * if that succeeds, BODY is executed, with WRAPPER bound to result of parsing
         WRAPPER, and PARSER bound to thunk to parse WRAPPIE.
         Additionally, CALL-PARSER is synonym to (FUNCALL PARSER),
         just to mimick CALL-TRANSFORM of original :AROUND clause.

    Typical use-case would be:

        (defrule my-wrapping-rule (wrap wrapper wrappie)
          (:wrap-around (let ((context-var-1 (car wrapper)) ; set dynamic state based on WRAPPER
                              (context-var-2 (cadr wrapper)))
                          (call-parser))) ; trigger further parsing
          (:lambda (lst) ; LST here is result of parsing WRAPPIE
             (list :context `(,context-var-1 ,context-var-2) ; yep, we are inside the context
                   :content lst)))                           ; designated by the wrapper.

    If that sounds a little bit confusing, see test-suite DYNAMIC-WRAPPING
    in TESTS.LISP and EXAMPLE-VERY-CONTEXT-SENSITIVE.LISP for examples.
    This feature was introduced to express rules for reading block-scalars in YaML,
    hence, see www.yaml.org for the specification of block scalars and the idea of why
    this feature is needed.

    Since no parsing of a WRAPPIE is occured at the time BODY is executed, nothing sensible
    can be bound to &BOUNDS.
"
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
     (case (car expression)
       (string
        (eval-string expression text position end))
       (and
        (eval-sequence expression text position end))
       (or
        (eval-ordered-choice expression text position end))
       (not
        (eval-negation expression text position end))
       (* (cond ((equal (length expression) 2) (eval-greedy-repetition expression text position end))
		(t (eval-times expression text position end))))
       (+
        (eval-greedy-positive-repetition expression text position end))
       (?
        (eval-optional expression text position end))
       (&
        (eval-followed-by expression text position end))
       (->
        (eval-followed-by-not-gen expression text position end))
       (<-
        (eval-preceded-by-not-gen expression text position end))
       (!
        (eval-not-followed-by expression text position end))
       (character-ranges
        (eval-character-ranges expression text position end))
       (cond
	 (eval-cond expression text position end))
       (t
        (if (symbolp (car expression))
	    (eval-semantic-predicate expression text position end)
            (invalid-expression-error expression)))))
    (t
     (invalid-expression-error expression))))

(defun compile-expression (expression)
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
     (case (car expression)
       (string
        (compile-string expression))
       (and
        (compile-sequence expression))
       (or
        (compile-ordered-choice expression))
       (not
        (compile-negation expression))
       (* (cond ((equal (length expression) 2) (compile-greedy-repetition expression))
		(t (compile-times expression))))
       (+
        (compile-greedy-positive-repetition expression))
       (?
        (compile-optional expression))
       (&
        (compile-followed-by expression))
       (->
        (compile-followed-by-not-gen expression))
       (<-
        (compile-preceded-by-not-gen expression))
       (!
        (compile-not-followed-by expression))
       (character-ranges
        (compile-character-ranges expression))
       (cond
	 (compile-cond expression))
       (t
        (if (symbolp (car expression))
	    (compile-semantic-predicate expression)
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

(defun eval-sequence (expression text position end)
  (with-expression (expression (and &rest subexprs))
    (let (results)
      (dolist (expr subexprs
               (make-result
                :position position
                :production (mapcar #'result-production (nreverse results))))
        (let ((result (eval-expression expr text position end)))
          (if (error-result-p result)
              (return (make-failed-parse
                       :expression expression
                       :position position
                       :detail result))
              (setf position (result-position result)))
          (push result results))))))

(defun compile-sequence (expression)
  (with-expression (expression (and &rest subexprs))
    (let ((functions (mapcar #'compile-expression subexprs)))
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
    (eval `(let ((function (compile-expression ',subexpr)))
	     (named-lambda compiled-times (text position end)
	       (let* ((last nil)
		      (results
		       (iter (for i from 1 to ,to)
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
		      :detail last))))))))


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
    (let ((result (eval-expression subexpr text position end)))
      (if (error-result-p result)
          (make-failed-parse
           :position position
           :expression expression
           :detail result)
          (make-result
           :position position)))))

(defun compile-followed-by-not-gen (expression)
  (with-expression (expression (-> subexpr))
    (let ((function (compile-expression subexpr)))
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
    (let ((result (eval-expression subexpr text (1- position) end)))
      (if (or (error-result-p result) (not (equal (result-position result) position)))
          (make-failed-parse
           :position position
           :expression expression
           :detail result)
          (make-result
           :position position)))))

(defun compile-preceded-by-not-gen (expression)
  (with-expression (expression (<- subexpr))
    (let ((function (compile-expression subexpr)))
      (named-lambda compiled-followed-by-not-gen (text position end)
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
