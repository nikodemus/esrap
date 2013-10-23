(in-package #:cl-user)

(defpackage :esrap
  (:use :cl :alexandria :defmacro-enhance :iterate)
  (:shadowing-import-from :rutils.string :strcat)
  #+sbcl
  (:lock t)
  (:export
   #:&bounds

   #:! #:? #:+ #:* #:& #:~ #:<- #:->
   #:character-ranges #:wrap #:tag

   #:add-rule
   #:register-context
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
