;;;; macro.lisp

;;;; This is a part of esrap-liquid TDPL for Common Lisp
;;;; Alexander Popolitov, 2013
;;;; For licence, see COPYING

(in-package #:esrap-liquid)

(defmacro! descend-with-rule (o!-sym &rest args)
  `(multiple-value-bind (,g!-it ,g!-got) (gethash ,o!-sym *rules*)
     ;; (format t "sym: ~a ~a~%" ,o!-sym position)
     (if (not ,g!-got)
         (error "Undefined rule: ~s" ,o!-sym)
         (multiple-value-bind (result new-position) (funcall ,g!-it text position end ,@args)
           ;; (format t "position before setting ~a, after setting would be ~a" position new-position)
           (setf position new-position)
           result))))

(defmacro with-esrap-reader-context (&body body)
  `(let ((char-reader (get-dispatch-macro-character #\# #\\))
          (string-reader (get-macro-character #\")))
      (with-dispatch-macro-character (#\# #\\ (esrap-char-reader char-reader))
        (with-macro-character (#\" (esrap-string-reader string-reader))
          (read-macrolet ((literal-char (esrap-literal-char-reader char-reader))
                          (literal-string (esrap-literal-string-reader string-reader))
                          (character-ranges (esrap-character-ranges char-reader)))
            ,@body)))))

(defmacro with-esrap-variable-transformer (&body body)
  `(let ((*variable-transformer* (lambda (sym)
                                   ;; KLUDGE to not parse lambda-lists in defrule args
                                   (if (equal "CHARACTER" (string sym))
                                       `(descend-with-rule 'character nil)
                                       `(descend-with-rule ',sym)))))
     ,@body))

(defun! make-rule-lambda (name args body &optional (env :current))
  (macroexpand-all-transforming-undefs
   `(named-lambda ,(intern (strcat "ESRAP-" name)) (text position end ,@args)
      (let ((,g!-position position))
        (declare (ignorable ,g!-position))
        (symbol-macrolet ((match-start ,g!-position)
                          (match-end position))
          (with-cached-result (,name position text ,@args)
            (values (progn ,@body)
                    position)))))
   :o!-env env))

(defmacro!! defrule (name args &body body)
    (with-esrap-reader-context
      (call-next-method))
  (with-esrap-variable-transformer
    `(setf (gethash ',name *rules*)
           ,(make-rule-lambda name args body))))

(defmacro! make-result (result &optional (length 0))
  ;; We must preserve the semantics, that computation of results occurs before increment of position
  `(let ((,g!-result ,result))
     (incf position ,length)
     (values ,g!-result position)))


(defmacro! || (&rest clauses)
  `(multiple-value-bind (,g!-result ,g!-position)
       ;; All this tricky business with BLOCK just for automatic POSITION tracking.
       (block ,g!-ordered-choice
         (let (,g!-parse-errors)
           ,@(mapcar (lambda (clause)
                       `(handler-case (let ((position position))
                                        ;; (format t "Im in ordered choice~%")
                                        (return-from ,g!-ordered-choice (values ,clause position)))
                          (simple-esrap-error (e) (push e ,g!-parse-errors))))
                     clauses)
           (fail-parse (joinl "~%"
                              (mapcar (lambda (x)
                                        (slot-value x 'reason))
                                      (nreverse ,g!-parse-errors))))))
     (setf position ,g!-position)
     ,g!-result))
  

(defmacro ! (expr)
  "Succeeds, whenever parsing of EXPR fails. Does not consume."
  `(progn (let ((position position))
            (handler-case ,expr
              (simple-esrap-error () nil)
              (:no-error (result &optional position)
                (declare (ignore result position))
                (fail-parse "Clause under non-consuming negation succeeded."))))
          (make-result t 0)))

(defmacro !! (expr)
  "Succeeds, whenever parsing of EXPR fails. Consumes, assumes than EXPR parses just one character."
  `(progn (let ((position position))
            (handler-case ,expr
              (simple-esrap-error () nil)
              (:no-error (result &optional position)
                (declare (ignore result position))
                (fail-parse "Clause under consuming negation succeeded."))))
          (if (equal end position)
              (fail-parse "Reached EOF while trying to consume character.")
              (make-result (char text position) 1))))

(defmacro! times (subexpr &key from upto exactly)
  (flet ((frob (condition)
           `(let (,g!-result)
              (iter ,(if (or upto exactly)
                         `(for ,g!-i from 1 to ,(or upto exactly)))
                    (multiple-value-bind (,g!-subresult ,g!-position)
                        (handler-case (let ((position position))
                                        (values ,subexpr position))
                          (simple-esrap-error () (finish)))
                      (if-first-time nil
                                     (if (equal ,g!-position position)
                                         (terminate)))
                      (push ,g!-subresult ,g!-result)
                      (setf position ,g!-position))
                    (finally (if ,condition
                                 (return (make-result (nreverse ,g!-result)))
                                 (fail-parse "Greedy repetition failed.")))))))
    (cond (exactly (if (or from upto)
                       (error "keywords :EXACTLY and :FROM/:UPTO are mutually exclusive.")
                       (frob `(equal (length ,g!-result) ,exactly))))
          (from (if upto
                    (frob `(and (>= (length ,g!-result) ,from)
                                (<= (length ,g!-result) ,upto)))
                    (frob `(>= (length ,g!-result) ,from))))
          (upto (frob `(<= (length ,g!-result) ,upto)))
          (t (frob t)))))
  
(defmacro postimes (subexpr)
  `(times ,subexpr :from 1))

(defmacro! pred (predicate subexpr)
  `(let ((,g!-it ,subexpr))
     (if (funcall ,predicate ,g!-it)
         ,g!-it
         (fail-parse "Predicate test failed"))))

(defmacro progm (start meat end)
  "Prog Middle."
  `(progn ,start (prog1 ,meat ,end)))

(defmacro! ? (subexpr)
  `(multiple-value-bind (,g!-result ,g!-position)
       (block ,g!-?
         (let ((position position))
           (handler-case ,subexpr
             (simple-esrap-error () nil)
             (:no-error (result) (return-from ,g!-? (make-result result))))
           (make-result nil)))
     (when ,g!-position
       (setf position ,g!-position))
     ,g!-result))

(defmacro & (subexpr)
  `(make-result (let ((position position))
                  ,subexpr)))

(defmacro -> (subexpr)
  `(progn (let ((position position))
            ,subexpr)
          (make-result nil)))

(defmacro! <- (subexpr)
  (if (and (symbolp subexpr) (equal (string subexpr) "SOF"))
      `(if (equal 0 position)
           (make-result nil)
           (fail-parse "not at start-of-file"))
      `(let ((,g!-old-position position)
             (position (1- position)))
         (let ((,g!-result ,subexpr))
           (if (equal ,g!-old-position position)
               (make-result nil)
               (fail-parse "Parsing of subexpr took more than 1 char."))))))

(defmacro! cond-parse (&rest clauses)
  `(|| ,@(mapcar (lambda (clause)
                   `(progn ,@clause))
                 clauses)))
