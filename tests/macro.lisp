(in-package :esrap-liquid-tests)

(enable-read-macro-tokens)
(cl-interpol:enable-interpol-syntax)

(define-esrap-env foo)
(define-esrap-env bar)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *old-print-case* *print-case*)
  (setf *print-case* :downcase)
  (register-foo-context foo-context-1 quux)
  (setf *print-case* *old-print-case*))
