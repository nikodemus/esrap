;;;; tests/package.lisp

;;;; This is a part of esrap-liquid TDPL for Common Lisp
;;;; Alexander Popolitov, 2013
;;;; For licence, see COPYING


(in-package :cl-user)

(defpackage :esrap-tests
  (:use :alexandria :cl :esrap :fiveam)
  (:shadowing-import-from :esrap "!" "!!")
  (:export #:run-tests))

(in-package :esrap-tests)

(defun run-tests ()
  (let ((results (run 'esrap)))
    (fiveam:explain! results)
    (unless (fiveam:results-status results)
      (error "Tests failed."))))

