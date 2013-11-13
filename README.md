ESRAP-LIQUID
============

Why shouldn't I use full Common Lisp while defining packrat parser rules?

It originated as a fork of ESRAP by Nikodemus Siivola (https://github.com/nikodemus/esrap),
but I quickly realized, that changes I wanted to make are so numerous, that in fact
it should be a separate project.

Original idea is in this article:

  * Bryan Ford, 2002, "Packrat Parsing: a Practical Linear Time
    Algorithm with Backtracking".

    http://pdos.csail.mit.edu/~baford/packrat/thesis/

What irked me is ESRAP:
  - poor support of context-sensitive grammars (I was trying to implement parsing of YAML)
    - specifically, when caching, context was not taken into considerations
  - interface for defining rules was very rigid: defining of a syntactic structure of a rule
    was done in a very limited DSL, extension of which required hacking of ESRAP itself
  - when I started hacking ESRAP few more subtle things occured to me:
    - custom codewalker was implemented to account for special syntax sugar
    - due to this fact, compiler macros had to be used to obtain reasonable speed,n
      which in turn prevented to define e.g. package-local rules.

That said, I started this project in an attempt to fix some of those drawbacks,
mainly, rigidity, hence the name suffix "LIQUID".

What it has now:
  - full support of context sensitivity: you can 'register' variables, which store the context,
    and their value is taken into account, while caching results
  - no limited duplication of codewalking - to give characters, strings and symbols, that define rules,
    'special' meaning, CL-READ-MACRO-TOKENS is used;
    hence, ability to use *whole* CL, while defining rules - ESRAP-LIQUID is a 'proper' deformation of CL
  - definition of a rule is not split into separate syntactic part (which can fail) and semantic part
    (which cannot fail and may contain costly operations).
    It is due to you, the writer, to perform costly operations in the end of rule flow, if you so wish
  - rules may depend on additional arguments (such as CHARACTER rule, which has optional parameter,
    that specifies, which character it matches).
    That said, syntax of DEFRULE is now pretty much like syntax of DEFUN

What's not yet done:
  - introspection features (description of a grammar)
  - case-insensitive terminals

Usage is best illustrated by examples, so here they are. For more examples,
see example-sexp.lisp, example-symbol-table.lisp, example-very-context-sensitive.lisp.
For even more real-life examples see my YAML parser https://github.com/mabragor/cl-yaclyaml.
It makes extensive use of features, not found in original ESRAP and it would be very hard to
implement otherwise.

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
 (character-ranges ranges) -- character ranges
 (& followed-by)           -- does not consume
 (-> followed-by-not-gen)  -- does not consume, produces NIL
 (<- preceded-by-not-gen)  -- succeeds, if preceeded by something of length 1, produces NIL
 (! not-followed-by)       -- does not consume
 (pred #'<predicate> expr) -- semantic parsing


Typical idioms:

```lisp
; succeed, when all subexpressions succeed, return list of those subexpressions
ESRAP-LIQUID> (parse '(list #\a #\b #\c) "abc")
(#\a #\b #\c)
3
; succeed, when all subexpression succeed, return only last subexpression
ESRAP-LIQUID> (parse '(progn #\a #\b #\c) "abc")
#\c
3
; succeed, when all subexpression succeed, return only first subexpression
ESRAP-LIQUID> (parse '(prog1 #\a #\b #\c) "abc")
#\a
3
```

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
ESRAP-LIQUID> (parse '(descend-with-rule 'foo-times 3) "foofoofoo")
("foo" "foo" "foo")
ESRAP-LIQUID> (parse '(descend-with-rule 'foo-times 4) "foofoofoo")
#<ESRAP-LIQUID::SIMPLE-ESRAP-ERROR "~a~%">.
```

OK, that's all for this first readme version.
