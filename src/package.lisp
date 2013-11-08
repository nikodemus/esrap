;;;; package.lisp

;;;; This is a part of esrap-liquid TDPL for Common Lisp
;;;; Alexander Popolitov, 2013
;;;; For licence, see COPYING

(in-package #:cl-user)

(defpackage :esrap
  (:use #:cl #:alexandria #:defmacro-enhance #:iterate #:cl-indeterminism #:cl-read-macro-tokens)
  (:shadowing-import-from #:rutils.string #:strcat)
  (:export
   #:enable-read-macro-tokens #:disable-read-macro-tokens
   #:! #:? #:& #:~ #:<- #:-> #:!! #:times #:postimes #:pred #:progm #:cond-parse
   #:match-start #:match-end
   #:literal-string #:literal-char
   #:register-context
   #:concat
   #:defrule #:descend-with-rule #:any-string #:character #:string #:||
   #:parse
   #:text
   ))
