;;;; tests/tests.lisp

;;;; This is a part of esrap-liquid TDPL for Common Lisp
;;;; Alexander Popolitov, 2013
;;;; For licence, see COPYING

(in-package :esrap-liquid-tests)

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

(defmacro signals-esrap-error ((input position &optional messages) &body body)
  `(progn
     (signals (esrap-liquid::esrap-error)
       ,@body)
     (handler-case (progn ,@body)
       (esrap-liquid::esrap-error (condition)
         (is (string= (esrap-liquid::esrap-error-text condition) ,input))
         (is (= (esrap-liquid::esrap-error-position condition) ,position))
         ,@(when messages
                 `((let ((report (princ-to-string condition)))
                     ,@(mapcar (lambda (message)
                                 `(is (search ,message report)))
                               (ensure-list messages)))))))))

(test condition.1
  "Test signaling of `esrap-simple-parse-error' conditions for failed
   parses."
  (signals-esrap-error ("" 0 ("Greedy repetition failed"
                              "Encountered at"))
                       (parse 'integer ""))
  (signals-esrap-error ("123foo" 3 ("Clause under non-consuming negation succeeded"
                                    "Encountered at"))
                       (parse 'integer "123foo"))
  (signals-esrap-error ("1, " 1 ("Didnt make it to the end of the text"
                                 "Encountered at"))
                       (parse 'list-of-integers "1, ")))

(test condition.2
  "Test signaling of `left-recursion' condition."
  (signals (esrap-liquid::left-recursion)
    (parse 'left-recursion "l"))
  (handler-case (parse 'left-recursion "l")
    (esrap-liquid::left-recursion (condition)
      (is (string= (esrap-liquid::esrap-error-text condition) "l"))
      (is (= (esrap-liquid::esrap-error-position condition) 0))
      (is (eq (esrap-liquid::left-recursion-nonterminal condition)
              'left-recursion))
      (is (equal (esrap-liquid::left-recursion-path condition)
                 '(esrap-liquid::esrap-tmp-rule left-recursion left-recursion))))))

(test negation
  "Test negation in rules."
  (let* ((text "FooBazBar")
         (t1c (text (parse '(postimes (!! "Baz")) text :junk-allowed t)))
         (t1e (text (parse '(pred #'identity (postimes (!! "Baz"))) text :junk-allowed t)))
         (t2c (text (parse '(postimes (!! "Bar")) text :junk-allowed t)))
         (t2e (text (parse '(pred #'identity (postimes (!! "Bar"))) text :junk-allowed t)))
         (t3c (text (parse '(postimes (!! (|| "Bar" "Baz"))) text :junk-allowed t)))
         (t3e (text (parse '(pred #'identity (postimes (!! (|| "Bar" "Baz")))) text :junk-allowed t))))
    (is (equal "Foo" t1c))
    (is (equal "Foo" t1e))
    (is (equal "FooBaz" t2c))
    (is (equal "FooBaz" t2e))
    (is (equal "Foo" t3c))
    (is (equal "Foo" t3e))))


(test around.1 "Test executing code around the transform of a rule."
      (is (equal '((0) . "foo") (parse 'around.1 "foo")))
      (is (equal '((1 0) . "bar") (parse 'around.1 "{bar}")))
      (is (equal '((2 1 0) . "baz") (parse 'around.1 "{{baz}}"))))

(test around.2
  "Test executing code around the transform of a rule."
  (is (equal '(((0 . (0 . 3))) . "foo") (parse 'around.2 "foo")))
  (is (equal '(((1 . (0 . 5))
                (0 . (1 . 4))) . "bar") (parse 'around.2 "{bar}")))
  (is (equal '(((2 . (0 . 7))
                (1 . (1 . 6))
                (0 . (2 . 5)))
               . "baz") (parse 'around.2 "{{baz}}"))))

(test optional-test
  (is (equal '(#\b 2) (multiple-value-list (parse '(? (progn #\a #\b)) "ab"))))
  (is (equal '(nil 0) (multiple-value-list (parse '(? (progn #\a #\b)) "ac" :junk-allowed t))))
  (is (equal '(nil 0) (multiple-value-list (parse '(? (progn #\a #\b #\c)) "abd" :junk-allowed t)))))


(test character-range-test
  (is (equal '(#\a #\b) (parse '(times (character-ranges (#\a #\z) #\-)) "ab" :junk-allowed t)))
  (is (equal '(#\a #\b) (parse '(times (character-ranges (#\a #\z) #\-)) "ab1" :junk-allowed t)))
  (is (equal '(#\a #\b #\-) (parse '(times (character-ranges (#\a #\z) #\-)) "ab-" :junk-allowed t)))
  (is (equal nil (parse '(times (character-ranges (#\a #\z) #\-)) "AB-" :junk-allowed t)))
  (is (equal nil (parse '(times (character-ranges (#\a #\z) #\-)) "ZY-" :junk-allowed t)))
  (is (equal '(#\a #\b #\-) (parse '(times character-range) "ab-cd" :junk-allowed t))))


(test examples-from-readme-test
  (is (equal '("foo" 3)
             (multiple-value-list (parse '(|| "foo" "bar") "foo"))))
  (is (equal '(("foo" "foo" "foo") 9)
             (multiple-value-list (parse 'foo+ "foofoofoo"))))
  (is (eql 123 (parse '(pred #'oddp decimal) "123")))
  (is (equal '(nil 0)
             (multiple-value-list (parse '(pred #'evenp decimal) "123" :junk-allowed t)))))



;; TODO: I dunno, maybe I still should return NIL as a second value if parse succeeded without JUNK-ALLOWED?
(test ambiguous-greedy-repetitions
  (is (equal '((3) 3) (multiple-value-list (parse 'greedy-spaces "   "))))
  (is (equal '((3) 3) (multiple-value-list (parse 'greedy-pos-spaces "   ")))))


(test dynamic-wrapping			      
  (is (equal '(("oo" "oo" "oo") 10)
	     (multiple-value-list (parse 'simple-wrapped "foofoofoof"))))
  (is (equal '(("oofoofoof") 10)
	     (multiple-value-list (parse 'simple-wrapped "goofoofoof")))))

(test dynamic-times
  (is (equal '("aaaaa" "aaa") (parse 'dyna-from-tos "aaaaaaaa")))
  (is (equal '("aaaa" "aaaa") (let ((dyna-to-times 4))
				(parse 'dyna-from-tos "aaaaaaaa")))))

(test cond
  (is (equal "foo" (parse 'cond-word "aaaafoo")))
  (is (equal "foo" (let ((context t)) (parse '(progn context-sensitive word) "foo"))))
  (is (equal :error-occured (handler-case (parse '(progn context-sensitive word) "foo")
                              (error () :error-occured))))
  (is (equal "out of context word" (parse '(|| (progn context-sensitive word)
                                            ooc-word)
         				  "foo"))))

(test followed-by-not-gen
  (is (equal '("a" nil "b") (parse '(list "a" (-> "b") "b") "ab"))))

(test preceded-by-not-gen
  (is (equal '("a" nil "b") (parse '(list "a" (<- "a") "b") "ab"))))


(test esrap-env
  (is (equal "foo" (foo-parse 'abracadabra "")))
  (is (equal "bar" (bar-parse 'abracadabra "")))
  (signals (esrap-liquid::simple-error)
    (parse 'abracadabra "")))

(test rule-closures
  (is (equal :a (parse 'closure-rule "a")))
  (is (equal :b (parse 'closure-rule "b")))
  (is (equal :c (parse 'closure-rule "c"))))

