;;;; tests/tests.lisp

;;;; This is a part of esrap-liquid TDPL for Common Lisp
;;;; Alexander Popolitov, 2013
;;;; For licence, see COPYING

(in-package :esrap-tests)

(enable-read-macro-tokens)
(cl-interpol:enable-interpol-syntax)

(def-suite esrap)
(in-suite esrap)

(test basic
  (is (equal #\space (parse 'whitespace-char " ")))
  (is (equal #\tab (parse 'whitespace-char #?"\t")))
  (is (equal #\newline (parse 'whitespace-char #?"\n")))
  (is (equal #?"  \t\t\n\n" (parse 'whitespace #?"  \t\t\n\n")))
  (is (equal "asdf" (parse 'non-empty-line #?"asdf")))
  (is (equal "asdf" (parse 'non-empty-line #?"asdf\n")))
  )

(test smoke
  (is (equal '("1," "2," "" "3," "4.")
             (parse 'trimmed-lines "1,
                                    2,

                                    3,
                                    4.")))
  (is (eql 123 (parse 'integer "  123")))
  (is (eql 123 (parse 'integer "  123  ")))
  (is (eql 123 (parse 'integer "123  ")))
  (is (equal '(123 45 6789 0) (parse 'list-of-integers "123, 45  ,   6789, 0")))
  (is (equal '(123 45 6789 0) (parse 'list-of-integers "  123 ,45,6789, 0  "))))

(test bounds.1
  (is (equal '("foo[0-3]")
             (parse 'tokens/bounds.1 "foo")))
  (is (equal '("foo[0-3]" "bar[4-7]" "quux[11-15]")
             (parse 'tokens/bounds.1 "foo bar    quux"))))

(test bounds.2
  (is (equal '("foo(0-3)")
             (parse 'tokens/bounds.2 "foo")))
  (is (equal '("foo(0-3)" "bar(4-7)" "quux(11-15)")
             (parse 'tokens/bounds.2 "foo bar    quux"))))

;; (test condition.1
;;   "Test signaling of `esrap-simple-parse-error' conditions for failed
;;    parses."
;;   (macrolet
;;       ((signals-esrap-error ((input position &optional messages) &body body)
;;          `(progn
;;             (signals (esrap-error)
;;               ,@body)
;;             (handler-case (progn ,@body)
;;               (esrap-error (condition)
;;                 (is (string= (esrap-error-text condition) ,input))
;;                 (is (= (esrap-error-position condition) ,position))
;;                 ,@(when messages
;;                     `((let ((report (princ-to-string condition)))
;;                         ,@(mapcar (lambda (message)
;;                                     `(is (search ,message report)))
;;                                   (ensure-list messages))))))))))
;;     (signals-esrap-error ("" 0 ("Could not parse subexpression"
;;                                 "Encountered at"))
;;       (parse 'integer ""))
;;     (signals-esrap-error ("123foo" 3 ("Could not parse subexpression"
;;                                       "Encountered at"))
;;       (parse 'integer "123foo"))
;;     (signals-esrap-error ("1, " 1 ("Incomplete parse."
;;                                    "Encountered at"))
;;       (parse 'list-of-integers "1, "))))

;; (test condition.2
;;   "Test signaling of `left-recursion' condition."
;;   (signals (left-recursion)
;;     (parse 'left-recursion "l"))
;;   (handler-case (parse 'left-recursion "l")
;;     (left-recursion (condition)
;;       (is (string= (esrap-error-text condition) "l"))
;;       (is (= (esrap-error-position condition) 0))
;;       (is (eq (left-recursion-nonterminal condition)
;;               'left-recursion))
;;       (is (equal (left-recursion-path condition)
;;                  '(left-recursion left-recursion))))))

;; (test negation
;;   "Test negation in rules."
;;   (let* ((text "FooBazBar")
;;          (t1c (text (parse '(+ (not "Baz")) text :junk-allowed t)))
;;          (t1e (text (parse (identity '(+ (not "Baz"))) text :junk-allowed t)))
;;          (t2c (text (parse '(+ (not "Bar")) text :junk-allowed t)))
;;          (t2e (text (parse (identity '(+ (not "Bar"))) text :junk-allowed t)))
;;          (t3c (text (parse '(+ (not (or "Bar" "Baz"))) text :junk-allowed t)))
;;          (t3e (text (parse (identity '(+ (not (or "Bar" "Baz")))) text :junk-allowed t))))
;;     (is (equal "Foo" t1c))
;;     (is (equal "Foo" t1e))
;;     (is (equal "FooBaz" t2c))
;;     (is (equal "FooBaz" t2e))
;;     (is (equal "Foo" t3c))
;;     (is (equal "Foo" t3e))))

;; (test around.1
;;   "Test executing code around the transform of a rule."
;;   (macrolet ((test-case (input expected)
;;                `(is (equal (parse 'around.1 ,input) ,expected))))
;;     (test-case "foo"     '((0) . "foo"))
;;     (test-case "{bar}"   '((1 0) . "bar"))
;;     (test-case "{{baz}}" '((2 1 0) . "baz"))))

;; (test around.2
;;   "Test executing code around the transform of a rule."
;;   (macrolet ((test-case (input expected)
;;                `(is (equal (parse 'around.2 ,input) ,expected))))
;;     (test-case "foo"     '(((0 . (0 . 3)))
;;                            . "foo"))
;;     (test-case "{bar}"   '(((1 . (1 . 4))
;;                             (0 . (0 . 5)))
;;                            . "bar"))
;;     (test-case "{{baz}}" '(((2 . (2 . 5))
;;                             (1 . (1 . 6))
;;                             (0 . (0 . 7)))
;;                            . "baz"))))

;; (test character-range-test
;;   (is (equal '(#\a #\b) (parse '(* (character-ranges (#\a #\z) #\-)) "ab" :junk-allowed t)))
;;   (is (equal '(#\a #\b) (parse '(* (character-ranges (#\a #\z) #\-)) "ab1" :junk-allowed t)))
;;   (is (equal '(#\a #\b #\-) (parse '(* (character-ranges (#\a #\z) #\-)) "ab-" :junk-allowed t)))
;;   (is (not (parse '(* (character-ranges (#\a #\z) #\-)) "AB-" :junk-allowed t)))
;;   (is (not (parse '(* (character-ranges (#\a #\z) #\-)) "ZY-" :junk-allowed t)))
;;   (is (equal '(#\a #\b #\-) (parse '(* character-range) "ab-cd" :junk-allowed t))))

;; (test examples-from-readme-test
;;   (is (equal '("foo" nil)
;;              (multiple-value-list (parse '(or "foo" "bar") "foo"))))
;;   (is (eq 'foo+ (add-rule 'foo+
;;                           (make-instance 'rule :expression '(+ "foo")))))
;;   (is (equal '(("foo" "foo" "foo") nil)
;;              (multiple-value-list (parse 'foo+ "foofoofoo"))))
;;   (is (eq 'decimal
;;           (add-rule 'decimal
;;                     (make-instance 'rule
;;                                    :expression `(+ (or "0" "1" "2" "3" "4" "5" "6" "7"
;;                                                        "8" "9"))
;;                                    :transform (lambda (list start end)
;;                                                 (declare (ignore start end))
;;                                                 (parse-integer (format nil "~{~A~}" list)))))))
;;   (is (eql 123 (parse '(oddp decimal) "123")))
;;   (is (equal '(nil 0)
;;              (multiple-value-list (parse '(evenp decimal) "123" :junk-allowed t)))))


;; (test ambiguous-greedy-repetitions
;;   (is (equal '((3) nil) (multiple-value-list (parse 'greedy-spaces "   "))))
;;   (is (equal '((3) nil) (multiple-value-list (parse 'greedy-pos-spaces "   ")))))


;; (test dynamic-wrapping			      
;;   (is (equal '(("oo" "oo" "oo") nil)
;; 	     (multiple-value-list (parse 'simple-wrapped "foofoofoof"))))
;;   (is (equal '(("oofoofoof") nil)
;; 	     (multiple-value-list (parse 'simple-wrapped "goofoofoof")))))

;; (test dynamic-times
;;   (is (equal '("aaaaa" "aaa") (parse 'dyna-from-tos "aaaaaaaa")))
;;   (is (equal '("aaaa" "aaaa") (let ((dyna-to 4))
;; 				(parse 'dyna-from-tos "aaaaaaaa")))))

;; (defrule cond-word (cond (dyna-from-to word)))

;; (test cond
;;   (is (equal "foo" (parse 'cond-word "aaaafoo")))
;;   (is (equal "foo" (let ((context t)) (parse '(cond (context word)) "foo"))))
;;   (is (equal :error-occured (handler-case (parse '(cond (context word)) "foo")
;; 			      (error () :error-occured))))
;;   (is (equal "out of context word" (parse '(cond (context word) (t ooc-word))
;; 					  "foo"))))

;; (test followed-by-not-gen
;;   (is (equal '("a" nil "b") (parse '(and "a" (-> "b") "b") "ab"))))

;; (test preceded-by-not-gen
;;   (is (equal '("a" nil "b") (parse '(and "a" (<- "a") "b") "ab"))))

;; (test on-the-fly-tagging
;;   (is (equal '(:simple-tag "aaa") (parse '(tag :simple-tag "aaa") "aaa"))))

