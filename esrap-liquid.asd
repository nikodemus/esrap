;;;; A packrat parser, implemented without duplication of common lisp code-walker.

;;;; Heavily based on initial work of Nikodemus Siivola (https://github.com/nikodemus/esrap)
;;;; Most of the code is, however, rewritten.

;;;; For licence details, see COPYING

(defpackage :esrap-liquid-system
  (:use :cl :asdf))

(in-package :esrap-liquid-system)

(defsystem :esrap-liquid
  :version "2.1" ; odd minor version numbers are for unstable versions
  :description "A Packrat / Parsing Grammar / TDPL parser for Common Lisp."
  :licence "GPL"
  :depends-on (#:alexandria #:iterate #:cl-ppcre #:cl-interpol)
  :serial t
  :components ((:module "src"
                        :pathname "src/"
                        :serial t
                        :components ((:file "package")
                                     (:file "conditions")
                                     (:file "miscellany")
				     (:file "iterators")
                                     (:file "rule-storage")
                                     (:file "memoization")
                                     (:file "macro")
                                     (:file "esrap")
                                     (:file "basic-rules")
                                     (:file "esrap-env")
				     ))
               (:static-file "example-sexp.lisp")
               (:static-file "example-symbol-table.lisp")
               (:static-file "README")))

(defsystem :esrap-liquid-tests
  :description "Tests for ESRAP-LIQUID."
  :licence "GPL"
  :depends-on (#:esrap-liquid #:fiveam #:cl-interpol)
  :serial t
  :pathname "tests/"
  :components ((:file "package")
               (:file "macro")
               (:file "rules")
               (:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :esrap-liquid))))
  (load-system :esrap-liquid-tests)
  (funcall (intern "RUN-TESTS" :esrap-liquid-tests)))
