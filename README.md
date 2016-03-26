ESRAP-LIQUID
============

MAJOR API CHANGE IN VERSIONS 2.*: we no longer use codewalker, hence rule definitions
now compile much faster. The price to pay is that sometimes you need to specify
manually, that something is a sub-rule (see V-macrolet in source and section "API Change" in this README).

Packrat parser generator, with possibility to use full Common Lisp when defining parsing rules.

It started as a fork of ESRAP by Nikodemus Siivola (https://github.com/nikodemus/esrap),
which is a DSL with (somewhat) more rigid syntax, but (as of now) nicer error reports.

Original idea of a packrat parser generator is described in this article:

  * Bryan Ford, 2002, "Packrat Parsing: a Practical Linear Time
    Algorithm with Backtracking".

    http://pdos.csail.mit.edu/~baford/packrat/thesis/

Examples:

```lisp
; plain characters match to precisely that character in text
ESRAP-LIQUID> (parse '#\a "a")
#\a
1
ESRAP-LIQUID> (parse '#\a "b")
#<ESRAP-LIQUID::SIMPLE-ESRAP-ERROR "~a~%">.
```

```lisp
; same goes for plain strings
ESRAP-LIQUID> (parse '"foo" "foo")
"foo"
3
ESRAP-LIQUID> (parse '"foo" "bar")
#<ESRAP-LIQUID::SIMPLE-ESRAP-ERROR "~a~%">.
```

```lisp
; CHARACTER matches any single character
ESRAP-LIQUID> (parse 'character "a")
#\a
1
ESRAP-LIQUID> (parse 'character "b")
#\b
1
```

```lisp
; (ANY-STRING <LENGTH>) matches any string of a given length character
ESRAP-LIQUID> (parse '(any-string 3) "foo")
"foo"
3
ESRAP-LIQUID> (parse '(any-string 3) "bar")
"bar"
3
ESRAP-LIQUID> (parse '(any-string 3) "caboom!")
#<ESRAP-LIQUID::SIMPLE-ESRAP-ERROR "~a~%">.
```

```lisp
; (!! <EXPR>) matches, whenever EXPR fails and consumes one character
ESRAP-LIQUID> (parse '(!! "foo") "bar" :junk-allowed t)
#\b
1
ESRAP-LIQUID> (parse '(!! "foo") "foo")
#<ESRAP-LIQUID::SIMPLE-ESRAP-ERROR "~a~%">.
```

```lisp
; (|| &rest <EXPRS>) is an ordered choice, matches, whenever one of EXPRS succeeds, and outputs it
ESRAP-LIQUID> (parse '(|| #\a #\b) "a")
#\a
1
ESRAP-LIQUID> (parse '(|| #\a #\b) "b")
#\a
1
ESRAP-LIQUID> (parse '(|| #\a #\b) "c")
#<ESRAP-LIQUID::SIMPLE-ESRAP-ERROR "~a~%">.
```

```lisp
; (times <EXPR> &key from upto exactly) greedy matches expression multiple times, returns values as a list.
; If :UPTO or :EXACTLY is given, then consumes only that much exprs, even if there are more
; if :FROM is given, fails if consumed less than :FROM expressions
ESRAP-LIQUID> (parse '(times #\a) "")
NIL
0
ESRAP-LIQUID> (parse '(times #\a) "aaa")
(#\a #\a #\a)
3
ESRAP-LIQUID> (parse '(times #\a :exactly 6) "aaaaaa")
(#\a #\a #\a #\a #\a #\a)
6
ESRAP-LIQUID> (parse '(times #\a :exactly 6) "aaa")
#<ESRAP-LIQUID::SIMPLE-ESRAP-ERROR "~a~%">.
```

```lisp
; (postimes <EXPR>) is an alias for (times <EXPR> :from 1)
ESRAP-LIQUID> (parse '(postimes #\a) "")
#<ESRAP-LIQUID::SIMPLE-ESRAP-ERROR "~a~%">.
ESRAP-LIQUID> (parse '(postimes #\a) "aaa")
(#\a #\a #\a)
3
```

```lisp
; (? <EXPR>) returns result of parsing of EXPR, when parsing succeeds, and NIL otherwise, does not fail
ESRAP-LIQUID> (parse '(? #\a) "")
NIL
0
ESRAP-LIQUID> (parse '(? #\a) "a")
#\a
1
```

Other operators, defined by ESRAP-LIQUID, include:
 - (character-ranges ranges) -- succeeds, when next character fits into specified ranges
 - (& followed-by)           -- parses subclause, then rewinds iterator back, like PEEK-CHAR, but with possibly more complex expressions
 - (-> followed-by-not-gen)  -- same as &, but produces NIL
 - (<- preceded-by-not-gen)  -- succeeds, if preceeded by something of length 1, produces NIL
 - (! not-followed-by)       -- an antipode of &, also rewinds iterator
 - (pred #'<predicate> expr) -- succeeds, if #'<predicate> returns true
 - (most-full-parse &rest exprs) -- try to parse all subexpressions and choose the longest one
 - (v subexpr &rest args)    -- syntactic sugar for DESCEND-WITH-RULE

Typical idioms:

```lisp
; succeed, when all subexpressions succeed, return list of those subexpressions
; Note that now you need to explicitly write (V ...) in order to indicate that
; character means "try to parse this character"
ESRAP-LIQUID> (parse '(list (v #\a) (v #\b) (v #\c)) "abc")
(#\a #\b #\c)
3
; succeed, when all subexpression succeed, return only last subexpression
ESRAP-LIQUID> (parse '(progn (v #\a) (v #\b) (v #\c)) "abc")
#\c
3
; succeed, when all subexpression succeed, return only first subexpression
ESRAP-LIQUID> (parse '(prog1 (v #\a) (v #\b) (v #\c)) "abc")
#\a
3
```

For more examples,
see example-sexp.lisp, example-symbol-table.lisp, example-very-context-sensitive.lisp.
For more real-life examples see my YAML parser https://github.com/mabragor/cl-yaclyaml.
The parsing part uses ESRAP-LIQUID extensively, in particular, in ways different from
traditional ESRAP.


Defining rules
--------------

Of course, if you could only write one-liners with PARSE, it won't be interesting at all
So, you can define new rules using DEFRULE macro.

```lisp
ESRAP-LIQUID> (defrule foo+ ()
                (postimes "foo"))
ESRAP-LIQUID> (parse 'foo+ "foofoofoo")
("foo" "foo" "foo")
```

```lisp
; simple arguments to rules are also possible
ESRAP-LIQUID> (defrule foo-times (times)
                (times "foo" :exactly times))
ESRAP-LIQUID> (parse '(v foo-times 3) "foofoofoo")
("foo" "foo" "foo")
ESRAP-LIQUID> (parse '(v foo-times 4) "foofoofoo")
#<ESRAP-LIQUID::SIMPLE-ESRAP-ERROR "~a~%">.
```

Defining esrap-environments
---------------------------

To be written, main macro are: DEFINE-ESRAP-ENV and IN-ESRAP-ENV
Grep tests in order to see basic usage.

The feature is needed, if you want to define rules not in global *RULES* variable (the default),
but instead in local 'environment' variable. This way you may have several non-colliding sets
of rules defined at the same time.


Capturing-variables : CAP, RECAP, RECAP?
----------------------------------------

Analogously to capturing groups in regexps, it is possible to capture
results of parsing of named rules, to aid destructuring.

Example: instead of clumsy

```lisp
(define-rule dressed-rule-clumsy ()
  (prog1 (progn (v "foo") (v "bar") (v "baz")
                (v meat))
         (v "rest1") (v "rest2") (v "rest3")))
```

you may write something like

```lisp
(define-rule dressed-rule-elegant ()
  (v "foo") (v "bar") (v "baz") (cap 1 meat) (v "rest1") (v "rest2") (v "rest3")
  (recap 1))
```

I.e. result of parsing of rule with name MEAT is stashed with CAP macro and
then accessed using RECAP macro.

Difference between RECAP and RECAP? is that while the former fails parsing if
the requested key was not captured, the latter just produces NIL.

See tests for examples of usage.
Also see CL-MIZAR parsing.lisp, where this is used a lot.


Streaming
---------

Now I made critical morphing of the code. Now it can be used not only to parse strings,
but also streams and, in general, iterators of tokens.

Here I understand iterators Pythonic style, i.e. they are classes with defined
NEXT-ITER method (the __next__ method in Python), that throws
stop-iteration error (the StopIteration exception in Python) when there are
no more values.

Now the function PARSE (which accepts string) is just a wrapper around
more general function PARSE-TOKEN-ITER (which accepts iterator of tokens)

This is only the stub of fantastic possibilities it opens, but the
hard part (change of architechture) is over and only cosmetics remain, which
includes:
  - PARSE-STREAM function, which accepts stream
  - MK-PARSING-ITER, creates iterator, which (lazily) parses the token stream
    - this should not only parse with a fixed rule, but also with different
      rule each time, and with supplied iterator of rules to parse in turn
    - it should be *convenient* to implement
      - Lisp reader
      - TeX lexer + TeX parser (yes, it should be convenient to work not only
        on iterators of chars, but also on iterators of arbitrary tokens)
      - combined TeX + Lisp reader

API Change
----------

In versions 1.* of ESRAP-LIQUID, we used the following implicit conventions, when
defining rules:
  - character literals (#\<something>) were understood as "try to parse this character out of the stream"
  - string literals ("something") were understood as "try to parse this string out of the stream"
  - free variables were understood as "try to find ESRAP rule with this name and parse it out of the stream"

These conventions are very handy when defining rules. However, the way they were implemented,
is not optimal:
  - character- and string-literal conventions required a use of implementation-dependent CL-READ-MACRO-TOKENS,
    to introduce special reader syntax (which excluded use of ESRAP-LIQUID on unsupported implementations)
  - free-variable convention required use of a codewalker (HU.DWIM.WALKER was used).
    While acceptable for small rule sets, when using ESRAP-LIQUID for large (like VHDL) rule sets,
    compilation really took long time (couple of minutes) and a lot of memory (GIGABYTES!)


So now these conventions are not valid in the whole scope of DEFRULE, but only
inside special macrolets. This allowed not to depend on codewalker and on special reader conventions.
Now conventions are like this:
  - special V macrolet (down-arrow-macrolet) is used to indicate, that here a descent into subrule is meant
    ```lisp
    (defrule foo ()
       ...
       (v #\a) ; parse literal char #\a
       (v "asdf") ; parse literal string "asdf"
       (v sub-rule x) ; parse a sub-rule, giving it an argument X (optional)
       ...)
    ```
  - in a lot of places V-macrolet is implicitly assumed (in all macro, defined in ESRAP-LIQUID).
    thus
    ```lisp
    (times #\a) ; will be correctly understood as "parse 0 or more characters 'A'"
    ```
    but in CL-form PROGN (or LIST) we have to explicitly write V-macrolets
    ```lisp
    (progn (v a) (v b) (v c)) ; parse sub-rules a, b and c
    (list (v d) (v e) f) ; parse sub-rules d and e, return variable f
    ```

Thus, to make a transition of your ESRAP-LIQUID-using code to 2.* API, you need:
  - go over all DEFRULE's and place V-macrolets in some places
  - replace C!-vars (related to capturing) with CAP, RECAP, RECAP? macros (see above)


