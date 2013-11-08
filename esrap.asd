;;;; A packrat parser, implemented without duplication of common lisp code-walker.

;;;; Heavily based on initial work of Nikodemus Siivola (https://github.com/nikodemus/esrap)
;;;; Most of the code is, however, rewritten.

;;;; For licence details, see COPYING

(defpackage :esrap-system
  (:use :cl :asdf))

(in-package :esrap-system)

(defsystem :esrap
  :version "1.1" ; odd minor version numbers are for unstable versions
  :description "A Packrat / Parsing Grammar / TDPL parser for Common Lisp."
  :licence "GPL"
  :depends-on (:alexandria :defmacro-enhance :iterate :rutils :cl-indeterminism :cl-read-macro-tokens)
  :serial t
  :components ((:module "src"
                        :pathname "src/"
                        :serial t
                        :components ((:file "package")
                                     (:file "conditions")
                                     (:file "miscellany")
                                     (:file "memoization")
                                     (:file "rule-storage")
                                     (:file "macro")
                                     (:file "esrap")
                                     (:file "basic-rules")))
               (:static-file "example-sexp.lisp")
               (:static-file "example-symbol-table.lisp")
               (:static-file "README")))

(defsystem :esrap-tests
  :description "Tests for ESRAP."
  :licence "GPL"
  :depends-on (#:esrap #:fiveam #:cl-interpol)
  :serial t
  :pathname "tests/"
  :components ((:file "package")
               (:file "rules")
               (:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :esrap))))
  (load-system :esrap-tests)
  (funcall (intern "RUN-TESTS" :esrap-tests)))
