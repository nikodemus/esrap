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

What I wanted to improve in ESRAP:
  - add support of context-sensitive grammars (I was trying to implement parsing of YAML)
    - specifically, when caching, context should be taken into considerations
  - make interface for defining rules more flexible:
    every now and then I needed a new feature of rule-defining DSL and it
    required hacking of core ESRAP code
  - like so many DSL-projects out there, ESRAP implemented its own codewalker,
    and would *greatly* benfit from not doing so:
    - so I wanted to somehow reuse CL codewalker
    - in particular, this would allow definition of package-local rules and 
      switch from interpreter mode to compiler mode. Theoretically, this would
      make thing faster.

The adjective, which suits the most to what I wanted ESRAP to be is "liquid",
so I added it and started hacking...

What I was able to do until now:
  - full support of context sensitivity: you can 'register' variables, which store the context,
    and their value is taken into account, while caching results
  - full reuse of CL's codewalker.
    Special ESRAP syntax, which makes it so convenient in the first place,
    is achieved with help of CL-READ-MACRO-TOKENS library;
    hence, you are abile to use *whole* CL, while defining rules
  - definition of a rule is not split into separate syntactic part and semantic part
    It gives more flexibility, but also more opportunities to write suboptimal code
    (e.g. the costly semantic operations may be performed for discarded results)
  - rules may depend on additional arguments
    (for example, CHARACTER rule, which accepts character it should match to)
    So, syntax of DEFRULE is now very close to syntax of DEFUN
  - STREAMING!!! Currenly I'm teaching ESRAP-LIQUID to work with streams
    (and in general to parse lazily) Hence, soon it will be possible to actually
    implement Lisp-reader with it (i.e., concisely)
  - debugging is done by setting *DEBUG* variable to T and recompiling the package.
    After that every parse outputs to stdout a progress of parsing in nice indented way,
    which helps to untangle even most complicated bugs

What's not yet done:
  - introspection features (description of a grammar)
  - case-insensitive terminals
  - friendly parsing error reports

Here are some examples of use. For more examples,
see example-sexp.lisp, example-symbol-table.lisp, example-very-context-sensitive.lisp.
For more real-life examples see my YAML parser https://github.com/mabragor/cl-yaclyaml.
The parsing part uses ESRAP-LIQUID extensively, in particular, in ways different from
traditional ESRAP.

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
 - (character-ranges ranges) -- character ranges
 - (& followed-by)           -- does not consume
 - (-> followed-by-not-gen)  -- does not consume, produces NIL
 - (<- preceded-by-not-gen)  -- succeeds, if preceeded by something of length 1, produces NIL
 - (! not-followed-by)       -- does not consume
 - (pred #'<predicate> expr) -- semantic parsing
 - (most-full-parse &rest exprs) -- try to parse all subexpressions and choose the one than
                                    consumed most


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

Defining esrap-environments
---------------------------

To be written, main macro are: DEFINE-ESRAP-ENV and IN-ESRAP-ENV
Grep tests in order to see basic usage.

The feature is needed, if you want to define rules not in global *RULES* variable (the default),
but instead in local 'environment' variable. This way you may have several non-colliding sets
of rules defined at the same time.


Capturing-variables
-------------------

Analogously to capturing groups in regexps, it is possible to capture
results of parsing of named rules, to aid destructuring.

Example: instead of clumsy

```lisp
(define-rule dressed-rule-clumsy ()
  (prog1 (progn "foo" "bar" "baz"
                meat)
         "rest1" "rest2" "rest3"))
```

you may write something like

```lisp
(define-rule dressed-rule-elegant ()
  "foo" "bar" "baz" c!-1-meat "rest1" "rest2" "rest3"
  c!-1)
```

I.e. result of parsing of rule with name MEAT is stored in variable C!-1,
which is later accessed.

See tests for examples of usage.
Also see CL-MIZAR parsing.lisp, where this is used a lot.


Streaming
---------

Now I made critical morphing of the code, such that it is now usable to
parse not only strings of fixed length, but also streams and, in general,
iterators of tokens.

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
    