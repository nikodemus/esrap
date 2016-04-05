;;;; tests/tests.lisp

;;;; This is a part of esrap-liquid TDPL for Common Lisp
;;;; Alexander Popolitov, 2013
;;;; For licence, see COPYING

(in-package :esrap-liquid-tests)

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
  (is (equal "" (parse 'empty-line #?"\n")))
  (is (equal "asdf" (parse 'trimmed-line #?"asdf")))
  (is (equal "asdf" (parse 'trimmed-line #?"   asdf   ")))
  (is (equal '("1," "2," "" "3," "4.")
             (parse 'trimmed-lines "1,
                                    2,

                                    3,
                                    4.")))
  (is (eql 123 (parse 'integer "  123")))
  (is (eql 123 (parse 'integer "  123  ")))
  (is (eql 123 (parse 'integer "123  ")))
  (is (equal '(#\a #\a #\a) (parse 'triple-a "aaa")))
  (is (equal '(#\a #\b #\c) (parse 'abc-or-def "abc")))
  (is (equal '(#\d #\e #\f) (parse 'abc-or-def "def")))
  (is (equal '(123 45) (parse 'list-of-integers "123,45")))
  (is (equal '(1 2) (parse 'list-of-integers "1, 2")))
  (is (equal '(123 45 6789 0) (parse 'list-of-integers "123, 45  ,   6789, 0")))
  (is (equal '(123 45 6789 0) (parse 'list-of-integers "  123 ,45,6789, 0  "))))

;; (test bounds.1
;;   (is (equal '("foo[0-3]")
;;              (parse 'tokens/bounds.1 "foo")))
;;   (is (equal '("foo[0-3]" "bar[4-7]" "quux[11-15]")
;;              (parse 'tokens/bounds.1 "foo bar    quux"))))

;; (test bounds.2
;;   (is (equal '("foo(0-3)")
;;              (parse 'tokens/bounds.2 "foo")))
;;   (is (equal '("foo(0-3)" "bar(4-7)" "quux(11-15)")
;;              (parse 'tokens/bounds.2 "foo bar    quux"))))

(defmacro signals-esrap-error ((input position &optional messages) &body body)
  `(progn
     (signals (esrap-liquid::esrap-error)
       ,@body)
     (handler-case (progn ,@body)
       (esrap-liquid::esrap-error (condition)
         ;; (is (string= (esrap-liquid::esrap-error-text condition) ,input))
         ;; (is (= (esrap-liquid::esrap-error-position condition) ,position))
         ,@(when messages
                 `((let ((report (princ-to-string condition)))
                     ,@(mapcar (lambda (message)
                                 `(is (search ,message report)))
                               (ensure-list messages)))))))))

(test condition.1
  "Test signaling of `esrap-simple-parse-error' conditions for failed
   parses."
  (signals-esrap-error ("" 0 ("Greedy repetition failed"))
    (parse 'integer ""))
  (signals-esrap-error ("123foo" 3 ("Clause under non-consuming negation succeeded"))
    (parse 'integer "123foo"))
  (signals-esrap-error ("1, " 1 ("Didnt make it to the end of the text"))
    (parse 'list-of-integers "1, ")))

(test non-consuming-negation
  (is (equal "foo" (parse '(text (list (! "bar") (v "foo"))) "foo"))))

(test condition.2
  "Test signaling of `left-recursion' condition."
  (signals (esrap-liquid::left-recursion)
    (parse 'left-recursion "l"))
  (handler-case (parse 'left-recursion "l")
    (esrap-liquid::left-recursion (condition)
      (declare (ignorable condition))
      ;; (is (string= "l" (esrap-liquid::esrap-error-text condition)))
      ;; (is (= (esrap-liquid::esrap-error-position condition) 0))
      ;; (is (eq (esrap-liquid::left-recursion-nonterminal condition)
      ;;         'left-recursion))
      ;; (is (equal (esrap-liquid::left-recursion-path condition)
      ;;            '(esrap-liquid::esrap-tmp-rule left-recursion left-recursion)))
      (is (equal t t))
      )))

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

;; (test around.2
;;   "Test executing code around the transform of a rule."
;;   (is (equal '(((0 . (0 . 3))) . "foo") (parse 'around.2 "foo")))
;;   (is (equal '(((1 . (0 . 5))
;;                 (0 . (1 . 4))) . "bar") (parse 'around.2 "{bar}")))
;;   (is (equal '(((2 . (0 . 7))
;;                 (1 . (1 . 6))
;;                 (0 . (2 . 5)))
;;                . "baz") (parse 'around.2 "{{baz}}"))))

(test optional-test
  (is (equal '(#\b 2) (multiple-value-list (parse '(? (progn (v #\a) (v #\b))) "ab"))))
  (is (equal '(nil 0) (multiple-value-list (parse '(? (progn (v #\a) (v #\b))) "ac" :junk-allowed t))))
  (is (equal '(nil 0) (multiple-value-list (parse '(? (progn (v #\a) (v #\b) (v #\c))) "abd" :junk-allowed t)))))


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
  (is (equal "foo" (let ((context t)) (parse '(progn (v context-sensitive) (v word)) "foo"))))
  (is (equal :error-occured (handler-case (parse '(progn (v context-sensitive) (v word)) "foo")
                              (error () :error-occured))))
  (is (equal "out of context word" (parse '(|| (progn (v context-sensitive) (v word))
                                            ooc-word)
         				  "foo"))))

(test followed-by-not-gen
  (is (equal '("a" nil "b") (parse '(list (v "a") (-> "b") (v "b")) "ab"))))

(test preceded-by-not-gen
  (is (equal '("a" nil "b") (parse '(list (v "a") (<- "a") (v "b")) "ab")))
  (is (equal '("a" "b") (parse '(list (v "a") (|| (<- "b") (v "b"))) "ab")))
  (is (equal '(#\newline (esrap-liquid::eof)) (parse '(list (v #\newline)
						       (times (progn (<- #\newline) (v esrap-liquid::eof))))
						     #?"\n"))))


(test esrap-env
  (is (equal "foo" (foo-parse 'abracadabra "")))
  (is (equal "bar" (bar-parse 'abracadabra "")))
  (is (equal "foo" (foo-parse-stream 'abracadabra (make-string-input-stream ""))))
  (is (equal "bar" (bar-parse-stream 'abracadabra (make-string-input-stream ""))))
  (signals (esrap-liquid::simple-error)
    (parse 'abracadabra ""))
  (signals (esrap-liquid::simple-error)
    (parse-stream 'abracadabra (make-string-input-stream "")))
  )

(test rule-closures
  (is (equal :a (parse 'closure-rule "a")))
  (is (equal :b (parse 'closure-rule "b")))
  (is (equal :c (parse 'closure-rule "c"))))

(test variable-capturing
  (is (equal '("foo") (parse 'dressed-elegantly "barbarbarfoobarbarbar")))
  (is (equal '("foo" "foo") (parse 'dressed-elegantly "barbarbarfoofoobarbarbar")))
  (is (equal '("foo") (parse 'dressed-elegantly-2 "barbarbarfoobarbarbar")))
  (is (equal '("foo" "foo") (parse 'dressed-elegantly-2 "barbarbarfoofoobarbarbar")))
  (is (equal '("foo") (parse 'dressed-elegantly-2 "barbarfoobarbar")))
  (is (equal '("foo" "foo") (parse 'dressed-elegantly-2 "barbarfoofoobarbar")))
  (is (equal '(("bar" "bar") ("bar" "bar" "bar")) (parse 'cap-overwrite "barbarfoofoobarbarbar"))))
  

(test optional-rule-args
  (is (equal '("f" "f" "f") (parse 'f-opt-times "fff")))
  (is (equal '("f" "f" "f" "f") (parse '(v f-opt-times 4) "ffff")))
  (signals-esrap-error ("ffff" 3 ("Didnt make it to the end of the text"))
                       (parse 'f-opt-times "ffff"))
  (signals-esrap-error ("fff" 3 ("Greedy repetition failed"))
                       (parse '(v f-opt-times 4) "fff")))
  
(test recursive-capturing
  (is (equal '(#\1 #\2 #\3 #\4 #\5 nil) (parse 'recurcapturing "(1(2(3(4(5(a))))))"))))



;;; String iterators

(test simple-iterators
  (is (equal '(#\a #\b #\c #\d) (iter (for c in-iter (esrap-liquid::mk-string-iter "abcd"))
				      (collect c))))
  (is (equal '(#\a #\b #\a #\b #\c #\d #\c #\d)
	     (let ((iter (esrap-liquid::mk-cache-iter (esrap-liquid::mk-string-iter "abcd")))
		   lst)
	       (setf lst (iter (for c in-iter iter)
			       (for i from 0 to 1)
			       (collect c)))
	       (esrap-liquid::rewind iter)
	       (setf lst (append lst
				 (iter (for c in-iter iter)
				       (collect c))))
	       (esrap-liquid::rewind iter 2)
	       (setf lst (append lst
				 (iter (for c in-iter iter)
				       (collect c))))))))

(test start-of-file
  (is (equal "a" (parse '(progn (v esrap-liquid::sof) (v "a")) "a"))))

(test most-full-parse
  (is (equal "aaaaa" (parse '(text (most-full-parse (times #\a :exactly 3)
				    (times #\a :exactly 5)))
			    "aaaaa")))
  (is (equal "aaaaa" (parse-stream '(text (most-full-parse (times #\a :exactly 3)
					   (times #\a :exactly 5)))
				   (make-string-input-stream "aaaaa")))))

(test hint-calling-rule
  (is (equal '(nil x x nil)
	     (parse 'sample-hint-calling-rule "aaaa"))))

;;; esrap-env

;; (test esrap-env-print-case
;;   (is (eq :quux foo-context-1))
;;   (is-true (find-symbol "QUUX-FOO-CONTEXT-1-P")))
