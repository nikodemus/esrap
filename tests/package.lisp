;;;; tests/package.lisp

;;;; This is a part of esrap-liquid TDPL for Common Lisp
;;;; Alexander Popolitov, 2013
;;;; For licence, see COPYING


(in-package :cl-user)

;; package to test importing rules from other packages
(defpackage :esrap-liquid-tests-other
  (:use #:alexandria #:cl #:esrap-liquid #:fiveam #:iterate)
  (:shadowing-import-from #:esrap-liquid #:! #:!!)
  (:export #:quux-parse))

(defpackage :esrap-liquid-tests
  (:use #:alexandria #:cl #:esrap-liquid #:fiveam #:iterate)
  (:shadowing-import-from #:esrap-liquid #:! #:!!)
  (:shadowing-import-from #:esrap-liquid-tests-other #:quux-parse)
  (:export #:run-tests))


(in-package :esrap-liquid-tests)

(defun run-tests ()
  (let ((results (run 'esrap)))
    (fiveam:explain! results)
    (unless (fiveam:results-status results)
      (error "Tests failed."))))

