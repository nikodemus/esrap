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

Operates *both* on strings and streams -- see section "Streaming" below.

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

Usually you don't want to mix parsing rules for different projects -- you want
a mechanism to scope your rules in some way. For that you can use DEFINE-ESRAP-ENV macro.

```lisp
(define-esrap-env foobar)

;; Now we can define rules in 'foobar' scope
(define-foobar-rule asdf ()
   ... ; rule definition here)
```

DEFINE-ESRAP-ENV has a MAINLY-NON-CONTEXT key. If it's T, then DEFINE-$(NAME)-RULE
expands into DEF-NOCONTEXT-RULE, rather than DEFRULE.
It's still possible to get context-sensitive rules by using DEFINE-C-$(NAME)-RULE.
Vice versa, if MAINLY-NON-CONTEXT key is NIL (default), then non-context-sensitive rules
can be defined using DEFINE-NC-$(NAME)-RULE.



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

It's possible to parse both strings and streams. Main function for parsing streams
is PARSE-STREAM.

Example of usage:
```lisp
(parse-stream 'some-rule (make-string-input-stream "foobar") :junk-allowed t)
```

In general, both PARSE and PARSE-STREAMS are just wrappers around more
general PARSE-TOKEN-ITER -- which accepts an iterator of tokens.
Here, iterators are implemented "pythonic way", i.e. they are classes with
NEXT-ITER method (the __next__ method in Python), that throw
STOP-ITERATION condition (the StopIteration exception in Python) when there are
no more values.

Thus, if you want ESRAP-LIQUID to parse something other than string or stream,
just grep for how PARSE-TOKEN-ITER is used.

  Gotchas!
  ========

  -- Since Lisp streams only allow to unread one character, and ESRAP-LIQUID in general
     does quite a lot of look-ahead, it's not safe to use a stream somewhere else after
     it was fed to PARSE-STREAM (even when :junk-allowed is set to T). This is because
     you don't know, what state the stream ends up in.


When you define esrap environment using DEFINE-ESRAP-ENV, say, like
```lisp
(define-esrap-env foo)
```
you get both FOO-PARSE and FOO-PARSE-STREAM -- environment versions of main parsing functions.


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


