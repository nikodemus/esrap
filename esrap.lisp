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

  ;; Special symbols
  (:export
   #:&bounds

   #:! #:? #:+ #:* #:& #:~
   #:character-ranges

   #:*grammar*)

  ;; Conditions
  (:export
   #:grammar-not-found-name
   #:grammar-not-found-error
   #:grammar-not-found-warning

   #:rule-not-found-grammar
   #:rule-not-found-name
   #:rule-not-found-error
   #:rule-not-found-warning

   #:invalid-expression-expression
   #:invalid-expression-error
   #:invalid-expression-warning

   #:esrap-error
   #:esrap-error-position
   #:esrap-error-text

   #:left-recursion
   #:left-recursion-nonterminal
   #:left-recursion-path)

  ;; Functions
  (:export
   #:add-rule
   #:call-transform
   #:change-rule
   #:concat
   #:defgrammar
   #:defrule
   #:describe-grammar
   #:find-grammar
   #:find-rule
   #:grammar-name
   #:grammar-rules
   #:grammar-dependencies
   #:in-grammar
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

;;; CONDITIONS

(define-condition grammar-not-found (condition)
    ((name :initarg :name :reader grammar-not-found-name))
  (:default-initargs
   :name (required-argument :name)))

(defmethod print-object ((condition grammar-not-found) stream)
  (format stream "The grammar named ~S could not be found."
          (grammar-not-found-name condition)))

(define-condition grammar-not-found-error (error
                                           grammar-not-found)
  ()
  (:documentation
   "Signaled when a grammar is requested by name but cannot be
found."))

(define-condition grammar-not-found-warning (warning
                                             grammar-not-found)
  ()
  (:documentation
   "Signaled when it can be determined at compile time that a
requested grammar cannot be found."))

(define-condition rule-not-found (condition)
  ((grammar :initarg :grammar :reader rule-not-found-grammar)
   (name :initarg :name :reader rule-not-found-name))
  (:default-initargs
   :grammar (required-argument :grammar)
   :name (required-argument :name)))

(defmethod print-object ((condition rule-not-found) stream)
  (format stream "The rule named ~S could not be found~@[ in grammar ~A~]."
          (rule-not-found-name condition)
          (rule-not-found-grammar condition)))

(define-condition rule-not-found-error (error
                                        rule-not-found)
  ()
  (:documentation
   "Signaled when a rule is requested by name but cannot be found
within a particular grammar."))

(define-condition rule-not-found-warning (warning
                                          rule-not-found)
  ()
  (:documentation
   "Signaled when it can be determined at compile time that a
requested rule cannot be found within a particular grammar."))

(define-condition invalid-expression (condition)
  ((expression :initarg :expression :reader invalid-expression-expression))
  (:default-initargs
   :expression (required-argument :expression)))

(defmethod print-object ((condition invalid-expression) stream)
  (format stream "Invalid expression: ~S"
          (invalid-expression-expression condition)))

(define-condition invalid-expression-error (error
                                            invalid-expression)
  ()
  (:documentation
   "Signaled when an invalid expression is encountered."))

(defun invalid-expression-error (expression)
  (error 'invalid-expression-error :expression expression))

(define-condition invalid-expression-warning (warning
                                              invalid-expression)
  ()
  (:documentation
   "Signaled when it can be determined at compile time that a supplied
expression is invalid."))

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

(deftype grammar-designator ()
  "Things which can be coerced into STRINGs by CL:STRING can designate
grammars."
  '(or symbol string))

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
;;; Each grammar, i.e. a named set of rules, is represented as a
;;; GRAMMAR instance. Grammars are named by strings and can be
;;; retrieved by name. In these regards, grammars behave similarly to
;;; Common Lisp packages.
;;;
;;; Dependencies between grammars (:USE-relations) are managed via
;;; GRAMMAR-REFERENTS and GRAMMAR-USE. Circular dependencies are
;;; currently not allowed.
;;;
;;; For each rule, there is a RULE-CELL in GRAMMAR-RULES, whose %INFO
;;; slot has the function that implements the rule in car, and the
;;; rule object in CDR. A RULE object can be attached to only one
;;; non-terminal at a time, which is accessible via RULE-SYMBOL.

(defvar *grammars* (make-hash-table :test #'equal
                                    #+sbcl :synchronized #+sbcl t)
  "Global storage of grammar objects, indexed by STRINGified name.")

(defun find-grammar (grammar-designator
                     &key
                     (if-does-not-exist nil))
  "Finds and returns the grammar designated by GRAMMAR-DESIGNATOR.

IF-DOES-NOT-EXISTS determines the behavior in case no such grammar
exists. If NIL, NIL is returned, if a function, the function is called
with an error or warning condition object."
  (check-type grammar-designator grammar-designator)
  (check-type if-does-not-exist  (or null function symbol))

  (let ((key (string grammar-designator)))
    (or (gethash  key *grammars*)
        (typecase if-does-not-exist
          (null nil)
          (function
           (funcall if-does-not-exist
                    (make-condition
                     (cond
                       ((member if-does-not-exist `(warn ,#'warn))
                        'grammar-not-found-warning)
                       (t
                        'grammar-not-found-error))
                     :name key)))))))

(defun (setf find-grammar) (new-value grammar-designator
                            &key
                            if-does-not-exist)
  "Stores GRAMMAR under the name GRAMMAR-DESIGNATOR.

If GRAMMAR is NIL, deletes the grammar designated by
GRAMMAR-DESIGNATOR. This signals a continuable error if the designated
grammar is used by other grammars.

IF-DOES-NOT-EXISTS is accepted for parity with FIND-GRAMMAR and
ignored."
  (declare (ignore if-does-not-exist))
  (check-type grammar-designator grammar-designator)

  ;; TODO(jmoringe, 2012-12-30): not thread-safe
  (let* ((key      (string grammar-designator))
         (existing (find-grammar grammar-designator
                                 :if-does-not-exist nil)))
    (cond
      ;; When we are supposed to delete a grammar, find it and check
      ;; whether it is used by other grammars. If we can remove it,
      ;; dereference used grammars.
      ((null new-value)
       (when existing
         (when-let ((refs (grammar-referents existing)))
           (cerror "Force"
                   "Grammar ~A is used by other grammar~P:~% ~{~A~^, ~}"
                   existing (length refs) refs))
         ;; Remove rules to potentially derefence rules in used
         ;; grammars.
         (clear-rules existing)
         ;; Deference used grammars.
         (mapc (rcurry #'dereference-grammar existing)
               (grammar-use existing))
         ;; Remove the grammar object.
         (remhash key *grammars*)))

      ;; When we are replacing an existing grammar, we check added
      ;; and removed used grammars to detect cyclic usage relations
      ;; and recompile rules as needed.
      (existing
       (flet ((diff-used (left right)
                (set-difference (grammar-use left) (grammar-use right)
                                :test #'eq)))
         (let ((added-used   (diff-used new-value existing))
               (removed-used (diff-used existing new-value)))

           ;; If used grammars have been added, make sure none of
           ;; these would introduce a cyclic usage relation.
           (dolist (grammar added-used)
             (when (member existing (grammar-use-closure grammar)
                           :test #'eq)
               ;; TODO(jmoringe, 2013-01-21): determine actual path;
               ;; proper condition
               (error "~@<Circular dependency with path:~_~{~A~^ -> ~}~@:>"
                      (list existing grammar "..." existing))))

           ;; Update the existing grammar with the new use-list and
           ;; documentation. Keep existing rules.
           (setf (grammar-use existing) (grammar-use new-value)
                 (grammar-documentation existing) (grammar-documentation new-value))

           ;; When used grammars have been added, reference them and
           ;; recompile rules.
           (when added-used
             (format t "Use-list of ~A changed~%added: ~A~%" new-value added-used)
             (mapc (rcurry #'reference-grammar existing) added-used)

             (dolist (grammar (remove-duplicates
                               (append added-used
                                       (mapcan #'grammar-use-closure added-used))
                               :test #'eq))
               (format t "~2@TMaybe recompiling in ~A~%" grammar)
               (dolist (cell (hash-table-values (grammar-rules grammar)))
                 (when-let ((rule (cell-rule cell)))
                   (format t "~4@TMaybe recompiling ~A in ~A~%" rule grammar)
                   (update-rule-dependencies grammar cell (rule-symbol rule))))))

           ;; When used grammars have been removed, dereference
           ;; them. We have no choice but "detach" (dereference all
           ;; referenced rules) and recompile all rules.
           (when removed-used
             (format t "Use-list of ~A changed~%removed: ~A~%"
                     new-value removed-used)

             (format t "~2@TRecompiling in ~A~%" existing)
             (dolist (cell (hash-table-values (grammar-rules existing)))
               (when-let ((rule (cell-rule cell)))
                 (format t "~4@TRecompiling ~A in ~A~%" rule existing)
                 (let ((use-new (grammar-use existing))) ; TODO hack
                   (setf (grammar-use existing) (append use-new removed-used))
                   (dereference-rule-dependencies existing rule)
                   (setf (grammar-use existing) use-new))
                 (recompile-cell existing cell)))

             (mapc (rcurry #'dereference-grammar existing) removed-used))))
       existing)

      ;; When we are installing a new grammar, reference used grammars
      ;; and store it.
      (t
       (mapc (rcurry #'reference-grammar new-value)
             (grammar-use new-value))
       (setf (gethash key *grammars*) new-value)))))

(defun coerce-to-grammar (name-or-grammar &key (if-does-not-exist #'error))
  "Finds and returns the grammar object designated by NAME-OR-GRAMMAR.
If NAME-OR-GRAMMAR is a GRAMMAR, just returns it.

IF-DOES-NOT-EXISTS determines the behavior in case no such grammar
exists. If NIL, NIL is returned, if a function, the function is called
with an error or warning condition object."
  (etypecase name-or-grammar
    (grammar-designator
     (find-grammar name-or-grammar :if-does-not-exist if-does-not-exist))
    (grammar
     name-or-grammar)))

(defstruct (grammar (:conc-name grammar-)
                    (:constructor %make-grammar (name use &optional documentation)))
  (name (required-argument :name) :type string :read-only t)
  (rules (make-hash-table :test #'eq) :read-only t)
  (use nil :type list)
  (referents nil :type list)
  (documentation nil :type (or null string)))

(defun make-grammar (name &key use documentation)
  "Makes and returns a grammar named NAME with use list USE and DOCUMENTATION.

USE is a list of GRAMMAR-DESIGNATOR s designating the grammars used by
the new grammar. If one or more of these designators does not name a
grammar, an error is signaled.

Note that this function has global side effects: The returned grammar
is registered into the global grammar list and usage relations between
grammars are established."
  (check-type name grammar-designator)

  (let* ((name (string name))
         (used (mapcar #'coerce-to-grammar use))
         (grammar (%make-grammar name used documentation)))
    (setf (find-grammar name) grammar)))

(macrolet ((frob (name reader)
             `(defun ,name (grammar)
                ,(format nil "Return ~S-closure for GRAMMAR." reader)
                ;; This is only called on acyclic grammar
                ;; graphs. Therefore, we do not check for cycles.
                (labels ((recur (grammar)
                           (remove-duplicates
                            (cons grammar (mapcan #'recur (,reader grammar)))
                            :test #'eq)))
                  (mapcan #'recur (,reader grammar))))))
  (frob grammar-use-closure grammar-use)
  (frob grammar-referents-closure grammar-referents))

(defun reference-grammar (grammar referent)
  (check-type grammar grammar)
  (check-type referent grammar)
  (assert (not (eq grammar referent)))
  (pushnew referent (grammar-referents grammar))
  grammar)

(defun dereference-grammar (grammar referent)
  (check-type grammar grammar)
  (check-type referent grammar)
  (assert (not (eq grammar referent)))
  (setf (grammar-referents grammar)
        (delete referent (grammar-referents grammar)))
  grammar)

(defun grammar-dependencies (grammar)
  "Returns the names of the grammars which are used by GRAMMAR."
  (mapcar #'grammar-name (grammar-use grammar)))

(defmethod print-object ((object grammar) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (let ((use-count               (length (grammar-use object)))
          (use-closure-count       (length (grammar-use-closure object)))
          (referents-count         (length (grammar-referents object)))
          (referents-closure-count (length (grammar-referents-closure object))))
      (format stream "~S ~D rule~:P~
                     ~[~*~:;, uses ~:*~D~[~:; + ~:*~D~]~]~
                     ~[~*~:;, used by ~:*~D~[~:; + ~:*~D~]~]"
              (grammar-name object)
              (hash-table-count (grammar-rules object))
              use-count (- use-closure-count use-count)
              referents-count (- referents-closure-count referents-count)))))

(defmethod documentation ((object grammar) (type (eql t)))
  (grammar-documentation object))

(defmethod documentation ((object t) (type (eql 'grammar)))
  (when-let ((grammar (coerce-to-grammar object :if-does-not-exist nil)))
    (documentation grammar t)))

(defun clear-rules (grammar)
  ;; Remove rules to potentially derefence rules in used grammars.
  (mapc #'(lambda (cell)
            (when-let ((rule (cell-rule cell)))
              (remove-rule (rule-symbol rule) :grammar grammar :force t)))
        (hash-table-values (grammar-rules grammar)))
  (clrhash (grammar-rules grammar))
  nil)

(defstruct (rule-cell (:constructor
                       make-rule-cell
                       (symbol &aux (%info (cons (undefined-rule-function nil symbol) nil))))
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

(defun undefined-rule-function (grammar symbol)
  (lambda (&rest args)
    (declare (ignore args))
    (error 'rule-not-found-error :grammar grammar :name symbol)))

(defun find-rule-cell (grammar symbol &key (recursive t))
  (check-type grammar grammar)
  (check-type symbol nonterminal)
  ;; Try to find the rule designated by SYMBOL in GRAMMAR.
  (or (gethash symbol (grammar-rules grammar))
      ;; Then look in grammars used by GRAMMAR.
      (when recursive
        (some (rcurry #'find-rule-cell symbol) (grammar-use grammar)))))

(defun delete-rule-cell (grammar symbol)
  (check-type grammar grammar)
  (remhash symbol (grammar-rules grammar)))

(defun ensure-rule-cell (grammar symbol)
  (check-type grammar grammar)
  (check-type symbol nonterminal)
  ;; FIXME: Need to lock GRAMMAR-RULES.
  (or (find-rule-cell grammar symbol)
      (setf (gethash symbol (grammar-rules grammar))
            (make-rule-cell symbol))))

(defun reference-rule-cell (cell referent)
  (check-type cell rule-cell)
  (check-type referent (or null rule-cell))
  (assert (not (eq cell referent)))
  ;; REFERENT can be NIL when we are currently compiling an "inline"
  ;; expression.
  (when referent
    (pushnew referent (cell-referents cell)))
  cell)

(defun dereference-rule-cell (cell referent)
  (check-type cell rule-cell)
  (check-type referent rule-cell)
  (assert (not (eq cell referent)))
  (setf (cell-referents cell) (delete referent (cell-referents cell)))
  cell)

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
    :reader rule-transform)
   (%around
    :initarg :around
    :initform nil
    :reader rule-around)))

(defmethod shared-initialize :after ((rule rule) slots &key)
  (check-expression (rule-expression rule)))

(defun rule-symbol (rule)
  "Returns the nonterminal associated with the RULE, or NIL of the rule
is not attached to any nonterminal."
  (slot-value rule '%symbol))

(defun rule-detached-p (rule)
  "Returns non-NIL if RULE is not attached to any nonterminal."
  (not (rule-symbol rule)))

(defmethod print-object ((rule rule) stream)
  (print-unreadable-object (rule stream :type t :identity nil)
    (format stream "~:[(detached)~;~:*~S <-~] ~S"
            (rule-symbol rule)
            (rule-expression rule))))

(defun sort-dependencies (grammar symbol dependencies)
  (let ((symbols (delete symbol dependencies))
        (defined nil)
        (undefined nil))
    (dolist (sym symbols)
      (if (find-rule sym :grammar grammar)
          (push sym defined)
          (push sym undefined)))
    (values defined undefined)))

(defun rule-dependencies (rule &key (grammar *grammar*))
  "Returns the dependencies of the RULE: primary value is a list of defined
nonterminal symbols, and secondary value is a list of undefined nonterminal
symbols.

If supplied, GRAMMAR has to be be a GRAMMAR instance or some other
object designating a grammar (type GRAMMAR-DESIGNATOR). If GRAMMAR
does not designate a grammar, a GRAMMAR-NOT-FOUND-ERROR is signaled."
  (check-type rule nonterminal)
  (check-type grammar (or grammar grammar-designator))
  (let* ((grammar (coerce-to-grammar grammar))
         (rule    (find-rule rule :grammar grammar
                                  :if-does-not-exist #'error)))
    (sort-dependencies
     grammar (rule-symbol rule)
     (%expression-dependencies grammar (rule-expression rule) nil))))

(defun rule-direct-dependencies (grammar rule)
  (check-type grammar grammar)
  (check-type rule rule)
  (sort-dependencies
   grammar (rule-symbol rule)
   (%expression-direct-dependencies (rule-expression rule) nil)))

(defun %rule-direct-dependencies (rule)
  (check-type rule rule)
  (delete (rule-symbol rule)
          (%expression-direct-dependencies (rule-expression rule) nil)))

(declaim (special *grammar*)
         (type grammar *grammar*))

(defvar *grammar* (make-grammar '#:default)
  "The \"current\" grammar object.

The value of this variable is used by DEFRULE if a grammar is not
supplied explicitly.

The initial global value is a grammar named \"DEFAULT\".")

(defmacro defgrammar (name &body options)
  "Define a new grammar named NAME, syntactically similar to
CL:DEFPACKAGE. OPTIONS can be any of the following:

* (:USE GRAMMAR-DESIGNATOR+)

  Allow using rules defined in the grammar(s) designated by
  GRAMMAR-DESIGNATOR+ in the grammar being defined.

* (:DOCUMENTATION STRING)

  Install STRING as the documentation string of the grammar being
  defined.

The :USE option can be supplied multiple times.

When used grammars are cannot be found at compile time, a full warning
is signaled. When used grammars are cannot be found at runtime, an
error is signaled.

Grammars can be redefined in a similar way packages can: if the
redefinition would introduce an error (e.g. a used grammar cannot be
found), the previous definition is kept. When the redefinition
succeeds, existing rules are retained but may behave differently since
added/removed used grammars can cause nonterminals to become defined
or undefined."
  (check-type name grammar-designator)

  (let ((use '())
        (documentation nil))
    ;; Build and check use list. If used grammars cannot be found,
    ;; still expand, but signal full warnings.
    (dolist (option options)
      (destructuring-bind (keyword &rest value) option
        (ecase keyword
          (:use
           (dolist (used value)
             (coerce-to-grammar used :if-does-not-exist #'warn))
           (appendf use value))
          (:documentation
           (if documentation
               (error "~@<~S supplied more than once.~@:>"
                      keyword)
               (setf documentation (first value)))))))

    ;; MAKE-GRAMMAR signals an error at runtime if a used grammar
    ;; cannot be found.
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (make-grammar ,(string name)
                     :use ',use
                     :documentation ,documentation))))

(defmacro in-grammar (name)
  "Set *GRAMMAR* to the grammar named NAME.

The grammar NAME has to exist at compile time of the IN-GRAMMAR form."
  (check-type name grammar-designator)

  ;; Signal a full warning if the grammar designated by NAME cannot be
  ;; found at compile time.
  (coerce-to-grammar name :if-does-not-exist #'warn)

  ;; Signal an error at runtime, if the grammar cannot be found at
  ;; runtime.
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *grammar* (coerce-to-grammar ,(string name)))))

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

(defun parse (expression text &key (grammar *grammar*)
                                   (start 0) end junk-allowed)
  "Parses TEXT using EXPRESSION in GRAMMAR from START to END.

If supplied, GRAMMAR has to be be a GRAMMAR instance or some other
object designating a grammar (type GRAMMAR-DESIGNATOR). If GRAMMAR
does not designate a grammar, a GRAMMAR-NOT-FOUND-ERROR is signaled

Incomplete parses are allowed only if JUNK-ALLOWED is true.

If GRAMMAR and EXPRESSION are constant and either GRAMMAR cannot be
found at compile time or EXPRESSION is not valid within GRAMMAR at
compile time, a full warning is signaled at compile time."
  ;; There is no backtracking in the toplevel expression -- so there's
  ;; no point in compiling it as it will be executed only once -- unless
  ;; it's a constant, for which we have a compiler-macro.
  (let ((grammar (coerce-to-grammar grammar))
        (end (or end (length text))))
    (process-parse-result
     (let ((*cache* (make-cache)))
       (eval-expression grammar expression text start end))
     text
     end
     junk-allowed)))

(define-compiler-macro parse (&whole form expression &rest arguments
                              &environment env)
  ;; If GRAMMAR is constant, we try and see whether it exists at
  ;; compile time. We signal a full warning at compile time, if we
  ;; cannot find it.
  (let ((grammar (getf (rest arguments) :grammar)))
    (cond
      ;; GRAMMAR is constant => check it at compile time, then
      ;; decline.
      ((and grammar (constantp grammar env)
            (not (constantp expression env)))
       (coerce-to-grammar (eval grammar) :if-does-not-exist #'warn)
       form)

      ;; EXPRESSION is constant, but GRAMMAR is not => check
      ;; EXPRESSION at compile time, then decline.
      ((and (or (not grammar) (not (constantp grammar env)))
            (constantp expression env))
       (handler-case ;; TODO(jmoringe, 2013-01-27): ugly and also repeated in DEFRULE
           (check-expression (eval expression))
         (invalid-expression-error (condition)
           (warn 'invalid-expression-warning
                 :expression (invalid-expression-expression condition))))
       form)

      ;; GRAMMAR and EXPRESSION are constant => check both compile
      ;; time and compile EXPRESSION at load time.
      ((and grammar (constantp grammar env)
            (constantp expression env)
            (when-let ((grammar (coerce-to-grammar
                                 (eval grammar)
                                 :if-does-not-exist #'warn)))
              ;; We could find the grammar, so we can try and also check
              ;; EXPRESSION. Again, we signal a full warning at compile
              ;; time, if EXPRESSION seems invalid.
              (handler-case ;; TODO(jmoringe, 2013-01-27): ugly and also repeated in DEFRULE
                  ;;; TODO(jmoringe, 2013-01-21): CHECK-EXPRESSION should support :if-invalid
                  (progn (check-expression (eval expression) grammar) t)
                (rule-not-found-error (condition)
                  (warn 'rule-not-found-warning
                        :grammar (rule-not-found-grammar condition)
                        :name (rule-not-found-name condition)))
                (invalid-expression-error (condition)
                  (warn 'invalid-expression-warning
                        :expression (invalid-expression-expression condition))
                  nil))))

       ;; Emit code to compile EXPRESSION at load time.
       (with-gensyms (expr-fun)
         `(let ((,expr-fun (load-time-value
                            (compile-expression
                             (coerce-to-grammar ,grammar) ,expression))))
            ;; This inline-lambda here provides keyword defaults and
            ;; parsing, so the compiler-macro doesn't have to worry
            ;; about evaluation order.
            ((lambda (text &key grammar (start 0) end junk-allowed)
               (declare (ignore grammar))
               (let ((*cache* (make-cache))
                     (end (or end (length text))))
                 (process-parse-result
                  (funcall ,expr-fun text start end)
                  text
                  end
                  junk-allowed)))
             ,@arguments))))

      ;; Nothing interesting is constant => decline.
      (t form))))

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

(defmacro defrule (&whole form symbol-and-options expression &body options)
  "Define SYMBOL-AND-OPTIONS as a nonterminal, using EXPRESSION as associated parsing expression.

SYMBOL-AND-OPTIONS has to be one of:

  * SYMBOL

    Just names the new rule, no options.

  * (SYMBOL &KEY GRAMMAR)

    SYMBOL names the rule.

    GRAMMAR is a GRAMMAR-DESIGNATOR designating the grammar to which
    the new rule should be added.

When the specified containing grammar cannot be found at compile time,
a full warning is signaled. When the specified containing grammar
cannot be found at runtime, an error is signaled.

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
"
  (let* ((symbol-and-options (ensure-list symbol-and-options))
         (symbol (first symbol-and-options))
         (grammar-designator (getf (rest symbol-and-options) :grammar))
         (grammar (if (not grammar-designator)
                      ;; No :GRAMMAR option => use *GRAMMAR* at runtime.
                      ;; The runtime error should rarely happen since
                      ;; the type of *GRAMMAR* is declared.
                      `(or *grammar* (error "~@<No current grammar.~@:>"))
                      ;; Grammar name has to be GRAMMAR-DESIGNATOR,
                      ;; preferably of an existing grammar => check at
                      ;; and possibly signal a full warning at compile
                      ;; time.
                      (progn
                        (check-type grammar-designator grammar-designator)
                        (coerce-to-grammar
                         grammar-designator :if-does-not-exist #'warn)
                        `(coerce-to-grammar ,(string grammar-designator)))))
         (transform nil)
         (around nil)
         (guard t)
         (condition t)
         (guard-seen nil))

    ;; TODO(jmoringe, 2013-01-21): redundant; see parse compiler macro
    ;; Check EXPRESSION for errors detectable at compile time.
    (handler-case
        (check-expression expression)
      (invalid-expression-error (condition)
        (warn 'invalid-expression-warning
              :expression (invalid-expression-expression condition))))

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
                                 ,@forms)))))))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,(once-only (grammar)
          `(progn
             (when-let* ((cell (find-rule-cell ,grammar ',symbol :recursive nil))
                         (rule (cell-rule cell)))
               (dereference-rule-dependencies ,grammar rule))
             (add-rule ',symbol
                       (make-instance 'rule
                                      :expression ',expression
                                      :guard-expression ',guard
                                      :transform ,(or transform '#'identity/bounds)
                                      :around ,around
                                      :condition ,condition)
                       :grammar ,grammar))))))

(declaim (type (or null rule-cell) *current-cell*))

(defvar *current-cell* nil
  "Stores the RULE-CELL instance currently being compiled.")

(defun add-rule (symbol rule &key (grammar *grammar*))
  "Associates RULE with the nonterminal SYMBOL in GRAMMAR.

SYMBOL has to be a nonterminal symbol which should name the new
rule. Note that SYMBOL itself names the rule, not (STRING SYMBOL).

RULE has to be a RULE instance.

If supplied, GRAMMAR has to be be a GRAMMAR instance or some other
object designating a grammar (type GRAMMAR-DESIGNATOR). If GRAMMAR
does not designate a grammar, a GRAMMAR-NOT-FOUND-ERROR is signaled

Signals an error if the rule is already associated with a
nonterminal. If the symbol is already associated with a rule, the old
rule is removed first."
  ;; FIXME: This needs locking and WITHOUT-INTERRUPTS.
  (check-type grammar (or grammar grammar-designator))
  (check-type symbol nonterminal)

  ;; Refuse to associate RULE to SYMBOL in GRAMMAR if it is already
  ;; associate to some nonterminal.
  (unless (rule-detached-p rule)
    (error "~S is already associated with the nonterminal ~S -- remove it first."
           rule (rule-symbol rule)))

  ;; 1. Resolve GRAMMAR, if necessary
  ;; 2. Create a rule cell for RULE in GRAMMAR
  ;; 3. Compile RULE
  ;; 4. Update dependencies: the added RULE may make previously
  ;;    undefined nonterminals become defined in grammars USEing
  ;;    GRAMMAR.
  (let* ((grammar (coerce-to-grammar grammar))
         (*current-cell* (ensure-rule-cell grammar symbol))
         (function (compile-rule grammar
                                 symbol ;; TODO
                                 (rule-expression rule)
                                 (rule-condition rule)
                                 (rule-transform rule)
                                 (rule-around rule)))
         (trace-info (cell-trace-info *current-cell*)))
    (set-cell-info *current-cell* function rule)
    (setf (cell-trace-info *current-cell*) nil)
    (setf (slot-value rule '%symbol) symbol)
    (when trace-info
      (trace-rule symbol :grammar grammar :break (second trace-info)))

    (update-rule-dependencies grammar *current-cell* symbol))

  (format t "Added ~A~%" rule) ;; TODO

  ;; Return SYMBOL naming rule as handle to rule object.
  symbol)

(defun find-rule (symbol &key (grammar *grammar*) (if-does-not-exist nil))
  "Returns the rule designated by NAME in GRAMMAR or :USEd grammars.

SYMBOL has to be the nonterminal symbol naming the requested
rule. Note that SYMBOL itself names the rule, not (STRING SYMBOL).

If supplied, GRAMMAR has to be a GRAMMAR instance or some other object
designating a grammar (type GRAMMAR-DESIGNATOR). If GRAMMAR does not
designate a grammar, a GRAMMAR-NOT-FOUND-ERROR is signaled

:IF-DOES-NOT-EXISTS determines the behavior in case SYMBOL does not
name a rule in GRAMMAR. If NIL, NIL is returned, if a function, the
function is called with an error condition object."
  (check-type symbol nonterminal)
  (check-type grammar (or grammar grammar-designator))

  ;; Resolve GRAMMAR, then try to find the rule cell for SYMBOL. If
  ;; there is none, use IF-DOES-NOT-EXIST.
  (let ((grammar (coerce-to-grammar grammar)))
    (or (when-let ((cell (find-rule-cell grammar symbol)))
          (cell-rule cell))
        (etypecase if-does-not-exist
          (null nil)
          (function
           (funcall if-does-not-exist
                    (make-condition
                     (cond
                       ((member if-does-not-exist `(warn ,#'warn))
                        'rule-not-found-warning)
                       (t
                        'rule-not-found-error))
                     :grammar grammar :name symbol)))))))

(defun remove-rule (symbol &key (grammar *grammar*) force)
  "Makes the nonterminal SYMBOL undefined in GRAMMAR.

SYMBOL has to be the nonterminal symbol. If it does not name a rule in
GRAMMAR, nothing happens.

If supplied, GRAMMAR has to be a GRAMMAR instance or some other object
designating a grammar (type GRAMMAR-DESIGNATOR). If GRAMMAR does not
designate a grammar, a GRAMMAR-NOT-FOUND-ERROR is signaled

If SYMBOL is defined and already referred to by other rules (in
GRAMMAR itself or grammars :USEing GRAMMAR), an error is signalled
unless :FORCE is true."
  (check-type symbol nonterminal)
  (check-type grammar (or grammar grammar-designator))

  ;; FIXME: Lock and WITHOUT-INTERRUPTS.
  (let* ((grammar (coerce-to-grammar grammar))
         (cell (find-rule-cell grammar symbol :recursive nil))
         (rule (when cell (cell-rule cell)))
         (trace-info (when cell (cell-trace-info cell))))
    (when cell
      (flet ((frob ()
               (format t "Removing ~A~%" (or (cell-rule cell) cell))
               (set-cell-info cell (undefined-rule-function grammar symbol) nil)
               (when trace-info
                 (setf (cell-trace-info cell) (list (cell-%info cell) (second trace-info))))
               (when rule
                 (update-rule-dependencies grammar cell (rule-symbol rule))
                 (dereference-rule-dependencies grammar rule)
                 (setf (slot-value rule '%symbol) nil))))
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

(defun change-rule (symbol expression &key (grammar *grammar*))
  "Modifies the nonterminal SYMBOL in GRAMMAR to use EXPRESSION instead.

SYMBOL has to be the nonterminal symbol. If it does not name a rule in
GRAMMAR, an error is signaled.

If supplied, GRAMMAR has to be a GRAMMAR instance or some other object
designating a grammar (type GRAMMAR-DESIGNATOR). If GRAMMAR does not
designate a grammar, a GRAMMAR-NOT-FOUND-ERROR is signaled."
  (check-type grammar (or grammar grammar-designator))
  (check-type symbol nonterminal)

  ;; Resolve GRAMMAR and find the rule designated by SYMBOL. Then
  ;; temporarily remove the rule while it is being modified.
  (let* ((grammar (coerce-to-grammar grammar))
         (rule (remove-rule symbol :grammar grammar :force t)))
    (unless rule
      (error 'rule-not-found-error :grammar grammar :name symbol))
    (setf (rule-expression rule) expression)
    (add-rule symbol rule :grammar grammar)))

(defun dereference-rule-dependencies (grammar rule)
  "Find cells of RULE's direct dependencies and derefence them."
  (check-type grammar grammar)
  (check-type rule rule)

  (when-let ((cell (find-rule-cell grammar (rule-symbol rule)
                                   :recursive nil)))
    (dolist (dep (%rule-direct-dependencies rule))
      (when-let ((dep-cell (find-rule-cell grammar dep)))
        (let ((*print-level* 3)) ;; TODO
          (format t "Dereferencing ~A~%" (or (cell-rule dep-cell) dep-cell)))
        (dereference-rule-cell dep-cell cell)))))

(defun update-rule-dependencies (grammar cell symbol)
  "Updates dependencies of the rule (SYMBOL CELL) in GRAMMAR."
  (check-type grammar grammar)
  (check-type cell rule-cell)
  (check-type symbol nonterminal)

  ;; Main goals:
  ;; 1. Update rule cell references
  ;; 2. Collect list of rules which have to be recompiled
  (let ((recompile '()))

    ;; Check whether CELL is undefined in GRAMMAR (this happens when
    ;; the corresponding is being removed).
    (when (null (cdr (cell-%info cell))) ;; TODO maybe add RULE-DEFINED-P?
      (let ((*print-level* 3)) ;; TODO
        (format t "~A is now undefined in ~A~%" (or (cell-rule cell) cell) grammar))
      ;; CELL is undefined. Referent cells in referent grammars have
      ;; to be recompiled in order to create new undefined rules in
      ;; these grammars.
      (dolist (referent (cell-referents cell))
        (let ((*print-level* 3)) ;; TODO
          (format t "~2@TReferent ~A has to be recompiled~%" (or (cell-rule referent) referent)))
        #+no (format t "~6@TRule     ~A~%" (find-rule-cell grammar d :recursive nil))
        #+no (find-rule-cell grammar reference :recursive nil)
        (pushnew (list referent grammar) recompile)))

    ;; Find undefined rules in grammars using GRAMMAR. When these
    ;; become defined due to (SYMBOL CELL):
    ;; 1. Remove rule cells marking undefined rules
    ;; 2. Recompile dependent rules
    (dolist (referent (grammar-referents-closure grammar))
      (format t "Checking ~A~%" referent) ;; TODO

      (when-let ((cell (find-rule-cell referent symbol :recursive nil)))
        (format t "~2@TFound rule ~A~%" cell)
        (when (null (cdr (cell-%info cell)))
          (format t "~2@TRule is undefined~%")

          ;; TODO(jmoringe, 2012-11-20): remove-rule

          (dolist (d (cell-referents cell))
            (let ((*print-level* 3))
             (format t "~6@TReferent ~A~%" (or (cell-rule d) d)))

            (dereference-rule-cell cell d)
            #+no (find-rule-cell referent d :recursive nil)
            (pushnew (list d referent) recompile))

          (remove-rule symbol :grammar referent))))

    ;; Recompile rules discovered above.
    (dolist (cell-and-grammar recompile)
      (destructuring-bind (cell grammar) cell-and-grammar
        (recompile-cell grammar cell)))))

(defun recompile-cell (grammar cell)
  (let ((*print-level* 3)) ;; TODO(jmoringe, 2013-01-27): remove
    (format t "?Recompiling ~A~%~9@T~A~%" grammar (or (cell-rule cell) cell)))
  (when-let ((rule (cell-rule cell)))
    (let ((*print-level* 3))
      (format t "!Recompiling ~A~%~9@T~A~%" grammar (or (cell-rule cell) cell)))
    (set-cell-info cell
                   (compile-rule grammar
                                 (rule-symbol rule)
                                 (rule-expression rule)
                                 (rule-condition rule)
                                 (rule-transform rule)
                                 (rule-around rule))
                   rule)))

(defvar *trace-level* 0)

(defvar *trace-stack* nil)

(defun trace-rule (symbol &key (grammar *grammar*) recursive break)
  "Turn on tracing of nonterminal SYMBOL.

If supplied, GRAMMAR has to be a GRAMMAR instance or some other object
designating a grammar (type GRAMMAR-DESIGNATOR). If GRAMMAR does not
designate a grammar, a GRAMMAR-NOT-FOUND-ERROR is signaled.

If :RECURSIVE is true, turn on tracing for the whole grammar rooted at
SYMBOL.

If :BREAK is true, break is entered when the rule is invoked."
  (check-type symbol nonterminal)
  (check-type grammar (or grammar grammar-designator))

  (unless (member symbol *trace-stack* :test #'eq)
    (let* ((grammar (coerce-to-grammar grammar))
           (cell (find-rule-cell grammar symbol)))
      (unless cell
        (error 'rule-not-found-error :grammar grammar :name symbol))
      (when (cell-trace-info cell)
        (let ((*trace-stack* nil))
          (untrace-rule symbol :grammar grammar)))
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
            (trace-rule dep :grammar grammar :recursive t :break break))))
      t)))

(defun untrace-rule (symbol &key (grammar *grammar*) recursive break)
  "Turn off tracing of nonterminal SYMBOL.

If supplied, GRAMMAR has to be a GRAMMAR instance or some other object
designating a grammar (type GRAMMAR-DESIGNATOR). If GRAMMAR does not
designate a grammar, a GRAMMAR-NOT-FOUND-ERROR is signaled.

If :RECURSIVE is true, untraces the whole grammar rooted at SYMBOL.

:BREAK is ignored, and is provided only for symmetry with TRACE-RULE."
  (declare (ignore break))
  (check-type symbol nonterminal)
  (check-type grammar (or grammar grammar-designator))

  (unless (member symbol *trace-stack* :test #'eq)
    (let* ((grammar (coerce-to-grammar grammar))
           (cell (find-rule-cell grammar symbol)))
      (unless cell
        (error 'rule-not-found-error :grammar grammar :name symbol))
      (let ((trace-info (cell-trace-info cell)))
        (when trace-info
          (setf (cell-%info cell) (car trace-info)
                (cell-trace-info cell) nil))
        (when recursive
          (let ((*trace-stack* (cons symbol *trace-stack*)))
            (dolist (dep (%rule-direct-dependencies (cell-rule cell)))
              (untrace-rule dep :grammar grammar :recursive t))))))
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

(defun symbol-length (x)
  (length (symbol-name x)))

(defun describe-grammar (symbol &key (stream *standard-output*)
                                     (grammar *grammar*))
  "Prints the grammar tree rooted at nonterminal SYMBOL to STREAM for
human inspection.

If supplied, GRAMMAR has to be be a GRAMMAR instance or some other
object designating a grammar (type GRAMMAR-DESIGNATOR). If GRAMMAR
does not designate a grammar, a GRAMMAR-NOT-FOUND-ERROR is signaled"
  (check-type symbol nonterminal)
  (check-type grammar (or grammar grammar-designator))

  (let* ((grammar (coerce-to-grammar grammar))
         (rule (find-rule symbol :grammar grammar)))
    (cond ((not rule)
           (format stream "Symbol ~S is not a defined nonterminal." symbol))
          (t
           (format stream "~&Grammar ~S:~%" grammar)
           (multiple-value-bind (defined undefined)
               (rule-dependencies (rule-symbol rule) :grammar grammar)
             (let ((length
                     (+ 4 (reduce #'max (append defined undefined)
                                  :initial-value 0
                                  :key           #'symbol-length))))
               (format stream "~3T~A~VT<- ~S~@[ : ~S~]~%"
                       symbol length (rule-expression rule)
                       (when (rule-condition rule)
                         (rule-guard-expression rule)))
               (when defined
                 (dolist (s defined)
                   (let ((dep (find-rule s :grammar grammar)))
                     (format stream "~3T~A~VT<- ~S~@[ : ~S~]~%"
                            s length (rule-expression dep)
                            (when (rule-condition rule)
                              (rule-guard-expression rule))))))
               (when undefined
                 (format stream "~%Undefined nonterminal~P:~%~{~3T~S~%~}"
                         (length undefined) undefined))))))))

;;; COMPILING RULES

(defun compile-rule (grammar symbol expression condition transform around)
  (declare (type (or boolean function) condition transform around))
  (let ((function (compile-expression grammar expression))
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
                          (if around
                              (make-result
                               :position (result-position result)
                               :production (flet ((call-rule ()
                                                    (funcall transform
                                                             (result-production result)
                                                             position
                                                             (result-position result))))
                                             (funcall around position (result-position result) #'call-rule)))
                              (make-result
                               :position (result-position result)
                               :production (funcall transform
                                                    (result-production result)
                                                    position
                                                    (result-position result))))))))
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

(defun validate-character-range (range)
  (or
    (characterp range)
    (and
      (consp range)
      (consp (cdr range))
      (characterp (car range))
      (characterp (cadr range))
      (null (cddr range)))))

(defun check-expression (expression &optional grammar)
  (labels
      ((rec (expression)
         (or (typecase expression
               ((eql character)
                t)
               (terminal
                t)
               (nonterminal
                (if grammar
                    (find-rule expression :grammar grammar
                               :if-does-not-exist #'error)
                    t))
               (cons
                (case (car expression)
                  ((and or)
                   (and (every #'rec (cdr expression)) t))
                  ((nil)
                   nil)
                  (string
                   (and (cdr expression) (not (cddr expression))
                        (typep (second expression) 'array-length)))
                  (character-ranges
                   (every #'validate-character-range (rest expression)))
                  (t
                   (and (symbolp (car expression))
                        (cdr expression) (not (cddr expression))
                        (rec (second expression))))))
               (t
                nil))
             (invalid-expression-error expression))))
    (rec expression)))

(defun %expression-dependencies (grammar expression seen)
  (etypecase expression
    ((member character)
     seen)
    (terminal
     seen)
    (nonterminal
     (if (member expression seen :test #'eq)
         seen
         (let ((rule (find-rule expression :grammar grammar))
               (seen (cons expression seen)))
           (if rule
               (%expression-dependencies grammar (rule-expression rule) seen)
               seen))))
    (cons
     (case (car expression)
       ((string character-ranges)
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
        ;; TODO(jmoringe, 2013-01-27): hack until EXPRESSION-CASE gets integrated
        (if (integerp (second expression))
            (eval-string expression text position end)
            (invalid-expression-error expression)))
       (and
        (eval-sequence grammar expression text position end))
       (or
        (eval-ordered-choise grammar expression text position end))
       (not
        ;; TODO(jmoringe, 2013-01-27): hack until EXPRESSION-CASE gets integrated
        (if (length= 2 expression)
            (eval-negation grammar expression text position end)
            (invalid-expression-error expression)))
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
       (character-ranges
        ;; TODO(jmoringe, 2013-01-27): hack until EXPRESSION-CASE gets integrated
        (if (every #'validate-character-range (rest expression))
            (eval-character-ranges grammar expression text position end)
            (invalid-expression-error expression)))
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
       (not
        (compile-negation grammar expression))
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
       (character-ranges
        (compile-character-ranges grammar expression))
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
      (eval-expression
       grammar (rule-expression (find-rule symbol :grammar grammar)) text position end)
      (funcall (cell-function (ensure-rule-cell grammar symbol)) text position end)))

(defun compile-nonterminal (grammar symbol)
  (let ((cell (ensure-rule-cell grammar symbol)))
    (declare (rule-cell cell))
    (unless (eq cell *current-cell*)
      (reference-rule-cell cell *current-cell*))
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

(defun eval-negation (grammar expression text position end)
  (with-expression (expression (not subexpr))
    (flet ((eval-sub (text position end)
             (eval-expression grammar subexpr text position end)))
      (declare (dynamic-extent #'eval-sub))
      (exec-negation #'eval-sub expression text position end))))

(defun compile-negation (grammar expression)
  (with-expression (expression (not subexpr))
    (let ((sub (compile-expression grammar subexpr)))
      (named-lambda compiled-negation (text position end)
        (exec-negation sub expression text position end)))))

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

;;; Character ranges

(defun exec-character-ranges (grammar expression ranges text position end)
  (declare (ignore grammar))
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

(defun eval-character-ranges (grammar expression text position end)
  (with-expression (expression (character-ranges &rest ranges))
    (exec-character-ranges grammar expression ranges text position end)))

(defun compile-character-ranges (grammar expression)
  (with-expression (expression (character-ranges &rest ranges))
    (named-lambda compiled-character-ranges (text position end)
      (exec-character-ranges grammar expression ranges text position end))))

;;; Hints for SLIME

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
