;;;;  Copyright (c) 2007-2012 Nikodemus Siivola <nikodemus@sb-studio.net>
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

(in-package :cl-user)

(defpackage :esrap-tests
  (:use :alexandria :cl :esrap :eos)
  (:shadowing-import-from :esrap "!")
  (:export #:run-tests))

(in-package :esrap-tests)

(def-suite esrap)
(in-suite esrap)

;;; Some helper macros

(defmacro with-clean-outer-compilation-unit (&body body)
  ;; Keep outer compilation-unit report clean.
  `(let ((*error-output*    (make-broadcast-stream)))
     (with-compilation-unit (:override t)
       ,@body)))

(defmacro ignoring-compile-time-warnings ((condition) &body body)
  `(funcall
    (with-clean-outer-compilation-unit
        (handler-bind
            ((,condition #'muffle-warning))
          (compile nil '(lambda () ,@body))))))

(defmacro compilation-signals (condition &body body)
  `(with-clean-outer-compilation-unit
       (signals ,condition
         (compile nil '(lambda () ,@body)))))

(test defgrammar.smoke
  (defgrammar #:defgrammar.smoke
    (:documentation "test"))

  (is (equal (grammar-name (find-grammar '#:defgrammar.smoke))
             "DEFGRAMMAR.SMOKE"))
  (is (equal (documentation (find-grammar '#:defgrammar.smoke) t)
             "test")))

(test defgrammar.use-non-existent-grammar
  "Test :USEing a non-existent grammar in DEFGRAMMAR."
  ;; We should see a warning at compile time and an error at
  ;; runtime. Check both independently.

  ;; Compile to check full warning at compile time.
  (compilation-signals grammar-not-found-warning
    (defgrammar #:baz (:use #:does-not-exist)))

  ;; Compile, ignoring compile time warnings, then check runtime
  ;; error.
  (signals grammar-not-found-error
    (ignoring-compile-time-warnings (grammar-not-found-warning)
      (defgrammar #:baz (:use #:does-not-exist)))))

(test in-grammar.non-existent-grammar
  "Test IN-GRAMMAR on non-existent grammar."
  ;; See defgrammar.use-non-existent.
  (compilation-signals grammar-not-found-warning
    (in-grammar #:does-not-exist))

  (signals grammar-not-found-error
    (ignoring-compile-time-warnings (grammar-not-found-warning)
      (in-grammar #:does-not-exist))))

(test find-grammar.non-existent
  "Test FIND-GRAMMAR on non-existent grammar."
  (is (null (find-grammar '#:does-not-exist :if-does-not-exist nil)))
  (signals grammar-not-found-warning
    (find-grammar '#:does-not-exist :if-does-not-exist #'warn))
  (signals grammar-not-found-error
    (find-grammar '#:does-not-exist :if-does-not-exist #'error)))

(test defrule.non-existent-grammar
  "Test DEFRULE with non-existent grammar."
  ;; See defgrammar.use-non-existent.
  (compilation-signals grammar-not-found-warning
    (defrule (foo :grammar #:does-not-exist) "foo"))

  (signals grammar-not-found-error
    (ignoring-compile-time-warnings (grammar-not-found-warning)
      (defrule (foo :grammar #:does-not-exist) "foo"))))

(test defrule.check-expression
  "Test expression checking in DEFRULE."
  ;; Test conditions signaled by DEFRULE at compile time.
  (compilation-signals invalid-expression-warning
    (defrule foo '(~ 1)))
  (compilation-signals invalid-expression-warning
    (defrule (foo :grammar #:test) '(~ 1)))

  ;; Test conditions signaled by DEFRULE at runtime.
  (macrolet ((is-invalid-expr (&body body)
               `(signals invalid-expression-error
                  (ignoring-compile-time-warnings (invalid-expression-warning)
                    ,@body))))
    (is-invalid-expr (defrule foo '(~ 1)))
    (is-invalid-expr (defrule foo '(string)))
    (is-invalid-expr (defrule foo '(character-ranges 1)))
    (is-invalid-expr (defrule foo '(character-ranges (#\a))))
    (is-invalid-expr (defrule foo '(character-ranges (#\a #\b #\c))))
    (is-invalid-expr (defrule foo '(and (string))))
    (is-invalid-expr (defrule foo '(not)))))

;;; FIND-RULE.{ARGUMENT-CHECKING,NON-EXISTENT}

(defgrammar #:find-rule)
(defrule (foo :grammar #:find-rule) #\a)

(test find-rule.smoke
  (is (not (null (find-rule 'foo :grammar '#:find-rule))))
  (is (null (find-rule :foo :grammar '#:find-rule)))
  (is (null (find-rule '#:foo :grammar '#:find-rule))))

(test find-rule.argument-checking
  "Test argument checking in FIND-RULE."
  (signals grammar-not-found-error
    (find-rule 'does-not-matter :grammar '#:no-such-grammar)))

(test find-rule.non-existent
  "Test FIND-RULE with non-existent rule."
  (is (null (find-rule '#:does-not-exist :grammar '#:find-rule
                       :if-does-not-exist nil)))
  (signals rule-not-found-warning
    (find-rule '#:does-not-exist :grammar '#:find-rule
               :if-does-not-exist #'warn))
  (signals rule-not-found-error
    (find-rule '#:does-not-exist :grammar '#:find-rule
               :if-does-not-exist #'error)))

;;; REMOVE-RULE.ARGUMENT-CHECKING

(defgrammar #:remove-rule.argument-checking)

(test remove-rule.argument-checking
  "Test argument checking in REMOVE-RULE."
  (signals grammar-not-found-error
    (remove-rule 'does-not-matter :grammar '#:no-such-grammar))
  (is (null (remove-rule 'no-such-rule :grammar '#:remove-rule.argument-checking))))

;;; REMOVE-RULE.USED.{ERROR,CORRECT-ORDER,redefinition}

;; We define three rules across two grammars:
;; Grammar FOO:         B -uses-> A
;; Grammar BAR: C -uses-^
;; Then try remove rules A and B in different ways.
(defgrammar #:remove-rule.used.foo)
(defgrammar #:remove-rule.used.bar
  (:use #:remove-rule.used.foo))

(defun define-rules-for-used-test ()
  (defrule (a :grammar #:remove-rule.used.foo) (+ "a"))
  (defrule (b :grammar #:remove-rule.used.foo) a)
  (defrule (c :grammar #:remove-rule.used.bar) b))

(test remove-rule.used.error
  "Test error signaling when attempting to remove rules which are used
by other rules."
  (define-rules-for-used-test)

  ;; A in FOO is used by B in FOO.
  (signals error (remove-rule 'a :grammar '#:remove-rule.used.foo))
  ;; B in FOO is used by C in BAR.
  (signals error (remove-rule 'b :grammar '#:remove-rule.used.foo)))

(test remove-rule.used.correct-order
  "Test removing rules with dependencies in a safe order."
  (define-rules-for-used-test)

  ;; This order should work.
  (remove-rule 'c :grammar '#:remove-rule.used.bar)
  (remove-rule 'b :grammar '#:remove-rule.used.foo)
  (remove-rule 'a :grammar '#:remove-rule.used.foo))

(test remove-rule.used.redefinition
  "Test removing rules after redefinition which makes the removal
safe."
  (define-rules-for-used-test)

  ;; Redefine B in FOO to not use A in FOO, then delete A in FOO.
  (defrule (b :grammar #:remove-rule.used.foo) (or))
  (remove-rule 'a :grammar '#:remove-rule.used.foo)

  ;; Redefine C in BAR to not use B in FOO, then delete B in FOO.
  (defrule (c :grammar #:remove-rule.used.bar) (or))
  (remove-rule 'b :grammar '#:remove-rule.used.foo)

  ;; C in BAR can be removed without redefinition.
  (remove-rule 'c :grammar '#:remove-rule.used.bar))

;;; RULE-DEPENDENCIES.{SMOKE,CONDITIONS}

(defgrammar #:rule-dependencies)
(defrule (a :grammar #:rule-dependencies)
    #\a)
(defrule (b :grammar #:rule-dependencies)
    (or a #\b))
(defrule (c :grammar #:rule-dependencies)
    (or b #\c))
(defrule (d :grammar #:rule-dependencies)
    (or a b c))

(test rule-dependencies.smoke
  "Smoke test for RULE-DEPENDENCIES."
  ;; Test default grammar.
  (let ((*grammar* (find-grammar '#:rule-dependencies)))
    (is (set-equal (rule-dependencies 'd) '(a b c))))

  ;; Test explicitly specified grammar.
  (macrolet
      ((has-dependencies (rule grammar &rest expected)
         `(is (set-equal (rule-dependencies ',rule :grammar ',grammar)
                         '(,@expected)))))
    (has-dependencies d #:rule-dependencies a b c)
    (has-dependencies c #:rule-dependencies a b)
    (has-dependencies b #:rule-dependencies a)
    (has-dependencies a #:rule-dependencies)))

(test rule-dependencies.conditions
  "Test conditions signaled by RULE-DEPENDENCIES."
  (signals rule-not-found-error
    (rule-dependencies 'no-such-rule))
  (signals grammar-not-found-error
    (rule-dependencies 'does-not-matter :grammar '#:no-such-grammar)))

;;; CHANGE-RULE.{ARGUMENT-CHECKING,SMOKE}

(defgrammar #:change-rule)
(defrule (foo :grammar #:change-rule)
    bar)

(test change-rule.argument-checking
  "Test argument checking in CHANGE-RULE."
  (signals grammar-not-found-error
    (change-rule 'does-not-matter 'baz :grammar '#:no-such-grammar))
  (signals rule-not-found-error
    (change-rule 'no-such-rule 'baz :grammar '#:change-rule)))

(test change-rule.smoke
  "Smoke test for CHANGE-RULE."
  ;; TODO(jmoringe, 2013-01-27): should RULE-EXPRESSION accept rule and grammar designators?
  (is (equal (rule-expression (find-rule 'foo :grammar '#:change-rule)) 'bar))
  (change-rule 'foo 'baz :grammar '#:change-rule)
  (is (equal (rule-expression (find-rule 'foo :grammar '#:change-rule)) 'baz)))

;;; GRAMMAR-INCLUSION.SMOKE

;; We define two grammars, FOO and BAR, and make sure that BAR can use
;; rules defined in FOO.
(defgrammar #:foo)

(defrule (a :grammar #:foo)
    (+ "a"))

(defgrammar #:bar
  (:use #:foo))

(defrule (b :grammar #:bar)
    (or a (+ "b"))
  (:text t))

(test grammar-inclusion.smoke
  "Smoke test for parsing with grammars which have dependencies."
  (is (string= (parse 'b "aa" :grammar '#:bar) "aa"))
  (is (string= (parse 'b "bb" :grammar '#:bar) "bb")))

;; We define a minimal grammar now, to avoid compile-time warning. At
;; runtime, we delete everything and set it up again. This is required
;; for repeated execution of the test.
(defgrammar #:fez)
(defrule (d :grammar #:fez) (or))

(test grammar-inclusion.redefinition-accross-grammars
  "Test grammar redefinition for grammars with dependencies."
  ;; We define two grammars, BAZ and FEZ, and make sure that FEZ can use
  ;; rules defined in BAZ. Grammar FEZ uses rules A and B from BAZ. Rule
  ;; B is undefined in both grammars at first and defined in BAZ later.
  (eval '(progn
          (defrule (d :grammar #:fez) (or))

          (defgrammar #:baz)
          (remove-rule 'b :grammar '#:baz)
          (defrule (a :grammar #:baz)
              (+ "a"))

          (defgrammar #:fez (:use #:baz))
          (defrule (d :grammar #:fez)
              (or a (+ "c") b)
            (:text t))))

  ;; Before redefinition:
  (is (string= (parse 'd "aa" :grammar '#:fez) "aa"))
  (signals rule-not-found-error (parse 'd "AA" :grammar '#:fez))
  (is (string= (parse 'd "cc" :grammar '#:fez) "cc"))
  (signals rule-not-found-error (parse 'd "bb" :grammar '#:fez))

  ;; After redefinition of A in BAZ:
  (eval '(defrule (a :grammar #:baz) (+ "A")))

  (signals rule-not-found-error (parse 'd "aa" :grammar '#:fez) "aa")
  (is (string= (parse 'd "AA" :grammar '#:fez) "AA"))
  (is (string= (parse 'd "cc" :grammar '#:fez) "cc"))
  (signals rule-not-found-error (parse 'd "bb" :grammar '#:fez))

  ;; After redefinition of B in BAZ:
  (format t "Defining ~S in ~A~%"
          'b  '#:baz)
  (eval '(defrule (b :grammar #:baz) (+ "b")))

  (signals esrap-error (parse 'd "aa" :grammar '#:fez) "aa")
  (is (string= (parse 'd "AA" :grammar '#:fez) "AA"))
  (is (string= (parse 'd "cc" :grammar '#:fez) "cc"))
  (is (string= (parse 'd "bb" :grammar '#:fez) "bb")))

;;; GRAMMAR-INCLUSION.REDEFINITION-WITH-ADDED-USE

(defgrammar #:grammar-inclusion.redefinition-with-added-use.foo)
(defrule (a :grammar #:grammar-inclusion.redefinition-with-added-use.foo)
    "a")
(defgrammar #:grammar-inclusion.redefinition-with-added-use.bar)
(defrule (b :grammar #:grammar-inclusion.redefinition-with-added-use.bar)
    a)

(test grammar-inclusion.redefinition-with-added-use
  "Test redefining a grammar to use another upstream grammar."
  (signals rule-not-found-error
    (parse 'b "a" :grammar '#:grammar-inclusion.redefinition-with-added-use.bar))

  (defgrammar #:grammar-inclusion.redefinition-with-added-use.bar
    (:use #:grammar-inclusion.redefinition-with-added-use.foo))

  (is (string= (parse 'b "a" :grammar '#:grammar-inclusion.redefinition-with-added-use.bar) "a")))

;;; GRAMMAR-INCLUSION.REDEFINITION-WITH-REMOVED-USE

(defgrammar #:grammar-inclusion.redefinition-with-removed-use.foo)
(defrule (a :grammar #:grammar-inclusion.redefinition-with-removed-use.foo)
    "a")
(defgrammar #:grammar-inclusion.redefinition-with-removed-use.bar
  (:use #:grammar-inclusion.redefinition-with-removed-use.foo))
(defrule (b :grammar #:grammar-inclusion.redefinition-with-removed-use.bar)
    a)

(test grammar-inclusion.redefinition-with-removed-use
  "Test using a grammar and then redefining the downstream grammar to
no longer use the upstream grammar."
  (is (string= (parse 'b "a" :grammar '#:grammar-inclusion.redefinition-with-removed-use.bar) "a"))

  (defgrammar #:grammar-inclusion.redefinition-with-removed-use.bar)

  (signals rule-not-found-error
    (parse 'b "a" :grammar '#:grammar-inclusion.redefinition-with-removed-use.bar)))

;;; GRAMMAR-INCLUSION.CIRCULAR-DEPENDENCY

(test grammar-inclusion.circular-dependency
  "Check signaled error in case of cyclic dependencies between
grammars."
  (eval '(progn
          ;; Clear uses in case the test is run multiple times.
          (defgrammar #:grammar-inclusion.circular-dependency.a)
          (defgrammar #:grammar-inclusion.circular-dependency.b)

          ;; Now setup the circular dependency.
          (defgrammar #:grammar-inclusion.circular-dependency.b
            (:use #:grammar-inclusion.circular-dependency.a))
          (signals simple-error
            (defgrammar #:grammar-inclusion.circular-dependency.a
              (:use #:grammar-inclusion.circular-dependency.b))))))

;;; Simple test grammar

(defgrammar #:test)
(in-grammar #:test)

;; A few semantic predicates

(defun not-doublequote (char)
  (not (eql #\" char)))

(defun not-digit (char)
  (when (find-if-not #'digit-char-p char)
    t))

(defun not-newline (char)
  (not (eql #\newline char)))

(defun not-space (char)
  (not (eql #\space char)))

;; Utility rules

(defrule whitespace (+ (or #\space #\tab #\newline))
  (:text t))

(defrule empty-line #\newline
  (:constant ""))

(defrule non-empty-line (and (+ (not-newline character)) (? #\newline))
  (:destructure (text newline)
    (declare (ignore newline))
    (text text)))

(defrule line (or empty-line non-empty-line)
  (:identity t))

(defrule trimmed-line line
  (:lambda (line)
    (string-trim '(#\space #\tab) line)))

(defrule trimmed-lines (* trimmed-line)
  (:identity t))

(defrule digits (+ (digit-char-p character))
  (:text t))

(defrule integer (and (? whitespace)
                      digits
                      (and (? whitespace) (or (& #\,) (! character))))
  (:destructure (whitespace digits tail)
    (declare (ignore whitespace tail))
    (parse-integer digits)))

(defrule list-of-integers (+ (or (and integer #\, list-of-integers)
                                 integer))
  (:destructure (match)
    (if (integerp match)
        (list match)
        (destructuring-bind (int comma list) match
          (declare (ignore comma))
          (cons int list)))))

(test parse.check-expression
  "Test expression checking in DEFRULE."
  ;; The compiler-macro on PARSE should signal compile-time warnings
  ;; for invalid expressions.
  (compilation-signals invalid-expression-warning
    (parse '(~ 1) "a"))
  (compilation-signals invalid-expression-warning
    (parse '(~ 1) "a" :grammar '#:test))

  ;; When ignoring compile-time warnings, PARSE should signal errors
  ;; at runtime for invalid expressions.
  (macrolet ((is-invalid-expr (&body body)
               `(signals invalid-expression-error
                  (ignoring-compile-time-warnings (invalid-expression-warning)
                    ,@body))))
    (is-invalid-expr (parse '(~ 1) "a"))
    (is-invalid-expr (parse '(~ 1) "a" :grammar '#:test))
    (is-invalid-expr (parse '(string) "a"))
    (is-invalid-expr (parse '(character-ranges 1) "a"))
    (is-invalid-expr (parse '(character-ranges (#\a)) "a"))
    (is-invalid-expr (parse '(character-ranges (#\a #\b #\c)) "a"))
    (is-invalid-expr (parse '(and (string)) "a"))
    (is-invalid-expr (parse '(not) "a"))))

(test parse.smoke
  "Smoke test for PARSE."
  (is (equal '("1," "2," "" "3," "4.")
             (parse 'trimmed-lines "1,
                                    2,

                                    3,
                                    4."
                    :grammar '#:test)))
  (is (eql 123 (parse 'integer "  123" :grammar '#:test)))
  (is (eql 123 (parse 'integer "  123  " :grammar '#:test)))
  (is (eql 123 (parse 'integer "123  " :grammar '#:test)))
  (is (equal '(123 45 6789 0)
             (parse 'list-of-integers "123, 45  ,   6789, 0" :grammar '#:test)))
  (is (equal '(123 45 6789 0)
             (parse 'list-of-integers "  123 ,45,6789, 0  " :grammar '#:test))))

(test parse.default-grammar
  "Test PARSE with default grammar."
  (let ((*grammar* (find-grammar '#:test)))
    (is (eql 123 (parse 'integer "  123")))
    (is (eql 123 (parse 'integer "  123  ")))
    (is (eql 123 (parse 'integer "123  ")))))

(test parse.non-existent-grammar
  "Test compile-time and runtime conditions signaled by PARSE for
non-existent grammars."
  ;; See defgrammar.use-non-existent.
  (compilation-signals grammar-not-found-warning
    (parse 'does-not-matter "foo" :grammar '#:does-not-exist))

  (signals grammar-not-found-error
    (ignoring-compile-time-warnings (grammar-not-found-warning)
      (parse 'does-not-matter "foo" :grammar '#:does-not-exist))))

(defgrammar #:parse.non-existent-rule)

(test parse.non-existent-rule
  "Test compile-time and runtime conditions signaled by PARSE for
non-existent rules."
  ;; See defgrammar.use-non-existent.
  (compilation-signals rule-not-found-warning
    (parse 'does-not-exist "foo"
           :grammar '#:parse.non-existent-rule))
  (compilation-signals rule-not-found-warning
    (parse '(+ (or "a" does-not-exist)) "foo"
           :grammar '#:parse.non-existent-rule))

  (signals rule-not-found-error
    (ignoring-compile-time-warnings (rule-not-found-warning)
      (parse 'does-not-exist "foo"
             :grammar '#:parse.non-existent-rule)))
  (signals rule-not-found-error
    (ignoring-compile-time-warnings (rule-not-found-warning)
      (parse '(+ (or "a" does-not-exist)) "foo"
             :grammar '#:parse.non-existent-rule))))

(defrule single-token/bounds.1 (+ (not-space character))
  (:lambda (result &bounds start end)
    (format nil "~A[~S-~S]" (text result) start end)))

(defrule single-token/bounds.2 (and (not-space character) (* (not-space character)))
  (:destructure (first &rest rest &bounds start end)
    (format nil "~C~A(~S-~S)" first (text rest) start end)))

(defrule tokens/bounds.1 (and (? whitespace)
                              (or (and single-token/bounds.1 whitespace tokens/bounds.1)
                                  single-token/bounds.1))
  (:destructure (whitespace match)
    (declare (ignore whitespace))
    (if (stringp match)
        (list match)
        (destructuring-bind (token whitespace list) match
          (declare (ignore whitespace))
          (cons token list)))))

(defrule tokens/bounds.2 (and (? whitespace)
                              (or (and single-token/bounds.2 whitespace tokens/bounds.2)
                                  single-token/bounds.2))
  (:destructure (whitespace match)
    (declare (ignore whitespace))
    (if (stringp match)
        (list match)
        (destructuring-bind (token whitespace list) match
          (declare (ignore whitespace))
          (cons token list)))))

(defrule left-recursion (and left-recursion "l"))

(test parse.bounds.1
  "First test for PARSE with bounds retrieval."
  (is (equal '("foo[0-3]")
             (parse 'tokens/bounds.1 "foo" :grammar '#:test)))
  (is (equal '("foo[0-3]" "bar[4-7]" "quux[11-15]")
             (parse 'tokens/bounds.1 "foo bar    quux" :grammar '#:test))))

(test parse.bounds.2
  "Second test for PARSE with bounds retrieval."
  (is (equal '("foo(0-3)")
             (parse 'tokens/bounds.2 "foo" :grammar '#:test)))
  (is (equal '("foo(0-3)" "bar(4-7)" "quux(11-15)")
             (parse 'tokens/bounds.2 "foo bar    quux" :grammar '#:test))))

(test parse.condition.1
  "Test signaling of ESRAP-ERROR conditions for failed parses."
  (macrolet
      ((signals-esrap-error ((input position &optional messages) &body body)
         `(progn
            (signals (esrap-error)
              ,@body)
            (handler-case (progn ,@body)
              (esrap-error (condition)
                (is (string= (esrap-error-text condition) ,input))
                (is (= (esrap-error-position condition) ,position))
                ,@(when messages
                    `((let ((report (princ-to-string condition)))
                        ,@(mapcar (lambda (message)
                                    `(is (search ,message report)))
                                  (ensure-list messages))))))))))
    (signals-esrap-error ("" 0 ("Could not parse subexpression"
                                "Encountered at"))
      (parse 'integer "" :grammar '#:test))
    (signals-esrap-error ("123foo" 3 ("Could not parse subexpression"
                                      "Encountered at"))
      (parse 'integer "123foo" :grammar '#:test))
    (signals-esrap-error ("1, " 1 ("Incomplete parse."
                                   "Encountered at"))
      (parse 'list-of-integers "1, " :grammar '#:test))))

(test parse.condition.2
  "Test signaling of LEFT-RECURSION condition."
  (signals (left-recursion)
    (parse 'left-recursion "l" :grammar '#:test))
  (handler-case (parse 'left-recursion "l" :grammar '#:test)
    (left-recursion (condition)
      (is (string= (esrap-error-text condition) "l"))
      (is (= (esrap-error-position condition) 0))
      (is (eq (left-recursion-nonterminal condition)
              'left-recursion))
      (is (equal (left-recursion-path condition)
                 '(left-recursion left-recursion))))))

(test parse.negation
  "Test negation in rules."
  (let* ((text "FooBazBar")
         (t1c (text (parse '(+ (not "Baz")) text
                           :grammar '#:test :junk-allowed t)))
         (t1e (text (parse (identity '(+ (not "Baz"))) text
                           :grammar '#:test :junk-allowed t)))
         (t2c (text (parse '(+ (not "Bar")) text
                           :grammar '#:test :junk-allowed t)))
         (t2e (text (parse (identity '(+ (not "Bar"))) text
                           :grammar '#:test :junk-allowed t)))
         (t3c (text (parse '(+ (not (or "Bar" "Baz"))) text
                           :grammar '#:test :junk-allowed t)))
         (t3e (text (parse (identity '(+ (not (or "Bar" "Baz")))) text
                           :grammar '#:test :junk-allowed t))))
    (is (equal "Foo" t1c))
    (is (equal "Foo" t1e))
    (is (equal "FooBaz" t2c))
    (is (equal "FooBaz" t2e))
    (is (equal "Foo" t3c))
    (is (equal "Foo" t3e))))

(declaim (special *depth*))
(defvar *depth* nil)

(defrule around/inner
    (+ (alpha-char-p character))
  (:text t))

(defrule around.1
    (or around/inner
        (and #\{ around.1 #\}))
  (:lambda (thing)
    (if (stringp thing)
        (cons *depth* thing)
        (second thing)))
  (:around ()
    (let ((*depth* (if *depth*
                       (cons (1+ (first *depth*)) *depth*)
                       (list 0))))
      (call-transform))))

(defrule around.2
    (or around/inner
        (and #\{ around.2 #\}))
  (:lambda (thing)
    (if (stringp thing)
        (cons *depth* thing)
        (second thing)))
  (:around (&bounds start end)
    (let ((*depth* (if *depth*
                       (cons (cons (1+ (car (first *depth*))) (cons start end))
                             *depth*)
                       (list (cons 0 (cons start end))))))
      (call-transform))))

(test parse.around.1
  "Test executing code around the transform of a rule."
  (macrolet ((test-case (input expected)
               `(is (equal (parse 'around.1 ,input :grammar '#:test) ,expected))))
    (test-case "foo"     '((0) . "foo"))
    (test-case "{bar}"   '((1 0) . "bar"))
    (test-case "{{baz}}" '((2 1 0) . "baz"))))

(test parse.around.2
  "Test executing code around the transform of a rule."
  (macrolet ((test-case (input expected)
               `(is (equal (parse 'around.2 ,input :grammar '#:test) ,expected))))
    (test-case "foo"     '(((0 . (0 . 3)))
                           . "foo"))
    (test-case "{bar}"   '(((1 . (1 . 4))
                            (0 . (0 . 5)))
                           . "bar"))
    (test-case "{{baz}}" '(((2 . (2 . 5))
                            (1 . (1 . 6))
                            (0 . (0 . 7)))
                           . "baz"))))

(defrule character-range (character-ranges (#\a #\b) #\-))

(test parse.character-range.smoke
  "Smoke test for parsing character ranges."
  (is (equal '(#\a #\b) (parse '(* (character-ranges (#\a #\z) #\-)) "ab"
                               :grammar '#:test :junk-allowed t)))
  (is (equal '(#\a #\b) (parse '(* (character-ranges (#\a #\z) #\-)) "ab1"
                               :grammar '#:test :junk-allowed t)))
  (is (equal '(#\a #\b #\-) (parse '(* (character-ranges (#\a #\z) #\-)) "ab-"
                                   :grammar '#:test :junk-allowed t)))
  (is (not (parse '(* (character-ranges (#\a #\z) #\-)) "AB-"
                  :grammar '#:test :junk-allowed t)))
  (is (not (parse '(* (character-ranges (#\a #\z) #\-)) "ZY-"
                  :grammar '#:test :junk-allowed t)))
  (is (equal '(#\a #\b #\-) (parse '(* character-range) "ab-cd"
                                   :grammar '#:test :junk-allowed t))))

(defun run-tests ()
  (let ((results (run 'esrap)))
    (eos:explain! results)
    (unless (eos:results-status results)
      (error "Tests failed."))))
