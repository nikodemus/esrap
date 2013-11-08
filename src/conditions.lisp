;;;; conditions.lisp

;;;; This is a part of esrap-liquid TDPL for Common Lisp
;;;; Alexander Popolitov, 2013
;;;; For licence, see COPYING


(in-package :esrap)

(define-condition esrap-error (parse-error)
  ((text :initarg :text :initform nil :reader esrap-error-text)
   (position :initarg :position :initform nil :reader esrap-error-position)
   (reason :initarg :reason :initform nil :reader esrap-error-reason))
  (:documentation
   "Signaled when an Esrap parse fails. Use ESRAP-ERROR-TEXT to obtain the
string that was being parsed, and ESRAP-ERROR-POSITION the position at which
the error occurred."))

(defmethod print-object ((condition esrap-error) stream)
  (if *print-escape*
      (call-next-method)
      ;; FIXME: this looks like it won't do the right thing when used as part of a
      ;; logical block.
      (when (or (not *print-lines*) (> *print-lines* 1))
        (if-let ((text (esrap-error-text condition))
                 (position (esrap-error-position condition)))
                (let* ((line (count #\Newline text :end position))
                       (column (- position (or (position #\Newline text
                                                         :end position
                                                         :from-end t)
                                               0)
                                  1))
                       ;; FIXME: magic numbers
                       (start (or (position #\Newline text
                                            :start (max 0 (- position 32))
                                            :end (max 0 (- position 24))
                                            :from-end t)
                                  (max 0 (- position 24))))
                       (end (min (length text) (+ position 24)))
                       (newline (or (position #\Newline text
                                              :start start
                                              :end position
                                              :from-end t)
                                    start))
                       (*print-circle* nil))
                  (format stream "~2&~A~2&  Encountered at:~%    ~
                                  ~A~%    ~
                                  ~V@T^ (Line ~D, Column ~D, Position ~D)~%"
			  (if-let ((reason (esrap-error-reason condition)))
				  reason
				  "No particular reason")
                          (if (emptyp text)
                              ""
                              (subseq text start end))
                          (- position newline)
                          (1+ line) (1+ column)
                          position))
                (format stream "~2&  <text and position not available>")))))

(define-condition simple-esrap-error (esrap-error simple-condition) ())

(defmethod print-object :before ((condition simple-esrap-error) stream)
  (apply #'format stream
         (simple-condition-format-control condition)
         (simple-condition-format-arguments condition)))

(declaim (ftype (function (t t t &rest t) (values nil &optional))
                simple-esrap-error))
(defun simple-esrap-error (text position reason format-control &rest format-arguments)
  (error 'simple-esrap-error
         :text text
         :position position
	 :reason reason
         :format-control format-control
         :format-arguments format-arguments))

(defmacro fail-parse (&optional (reason "No particular reason.") &rest args)
  `(let ((reason (apply #'format `(nil ,,reason ,,@args))))
     (simple-esrap-error text position reason "~a~%" reason)))

(define-condition left-recursion (esrap-error)
  ((nonterminal :initarg :nonterminal :initform nil :reader left-recursion-nonterminal)
   (path :initarg :path :initform nil :reader left-recursion-path))
  (:documentation
   "Signaled when left recursion is detected during Esrap parsing.
LEFT-RECURSION-NONTERMINAL names the symbol for which left recursion was
detected, and LEFT-RECURSION-PATH lists nonterminals of which the left
recursion cycle consists."))

(defmethod print-object :before ((condition left-recursion) stream)
  (format stream "Left recursion in nonterminal ~S. ~_Path: ~
                  ~{~S~^ -> ~}"
          (left-recursion-nonterminal condition)
          (left-recursion-path condition)))
