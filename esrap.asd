;;;;  Copyright (c) 2007-2012 Nikodemus Siivola <nikodemus@sb-studio.net>
;;;;
;;;;  Permission is hereby granted, free of charge, to any person
;;;;  obtaining a copy of this software and associated documentation files
;;;;  (the "Software"), to deal in the Software without restriction,
;;;;  including without limitation the rights to use, copy, modify, merge,
;;;;  publish, distribute, sublicense, and/or sell copies of the Software,
;;;;  and to permit persons to whom the Software is furnished to do so,
;;;;  subject to the following conditions:
;;;;
;;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;;  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;;  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;;  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;;  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defpackage :esrap-system
  (:use :cl :asdf))

(in-package :esrap-system)

(defsystem :esrap
  :version "0.9"
  :description "A Packrat / Parsing Grammar / TDPL parser for Common Lisp."
  :licence "MIT"
  :depends-on (:alexandria)
  :components ((:file "esrap")
               (:static-file "example-sexp.lisp")
               (:static-file "example-symbol-table.lisp")
               (:static-file "README")))

(defsystem :esrap-tests
  :description "Tests for ESRAP."
  :licence "MIT"
  :depends-on (:esrap :eos)
  :components ((:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :esrap))))
  (load-system :esrap-tests)
  (funcall (intern "RUN-TESTS" :esrap-tests)))
