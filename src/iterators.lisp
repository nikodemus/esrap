
;;;; iterators.lisp

;;;; This is a part of esrap-liquid TDPL for Common Lisp
;;;; Alexander Popolitov, 2013
;;;; For licence, see COPYING

(in-package :esrap-liquid)

;; I want ESRAP to be able to conveniently handle TeX token stream.
;; For this I need a caching iterator, which does the following:
;; 1) next fetches item from underlying itetator, or from cache
;; 2) random access to cache is fast
;; 3) I can discard some items from cache on demand

(defparameter buffer-vector-start-length 10)

(defclass buffer-vector ()
  ((vector)
   (start-pointer :initform 0)))

(defmethod initialize-instance :after ((this buffer-vector) &key &allow-other-keys)
  (with-slots (vector) this
    (setf vector (make-array buffer-vector-start-length :adjustable t :fill-pointer t))))

(defgeneric soft-shrink (obj num-elts-discarded)
  (:documentation "Shrink buffer (and cache) the soft way, just moving the start pointer"))
(defgeneric hard-shrink (obj num-elts-discarded)
  (:documentation "Shrink buffer (and cache) the hard way, actually reallocating the buffer and cleaning up cache"))
(defgeneric buffer-push (elt obj)
  (:documentation "Place object in the end of the buffer, necessarily increasing its size"))
(defgeneric buffer-pop (obj)
  (:documentation "Pop the last element of the buffer, but not before the start pointer"))


(define-condition buffer-error (error)
  ((msg :initarg :msg :initform nil)))

(defun buffer-error (str &rest args)
  (error 'buffer-error :msg (apply #'format (append (list nil str)
						    args))))


(defmethod soft-shrink ((obj buffer-vector) (num-elts-discarded integer))
  (with-slots (vector start-pointer) obj
    (let ((fill-pointer (fill-pointer vector)))
      (if (> (+ start-pointer num-elts-discarded) fill-pointer)
	  (buffer-error "Attempt to soft-shrink buffer more than its active length")
	  (incf start-pointer num-elts-discarded)))))

(defun calc-new-buffer-length (old-buffer-vector start-pointer)
  (let ((full-array-length (array-dimension old-buffer-vector 0))
	(actual-length (- (fill-pointer old-buffer-vector) start-pointer)))
    (if (> actual-length (/ full-array-length 2))
	full-array-length
	(1+ (floor full-array-length 2)))))
	

(defmethod hard-shrink ((obj buffer-vector) (num-elts-discarded integer))
  (with-slots (vector start-pointer) obj
    (let ((new-vector (make-array (calc-new-buffer-length vector start-pointer) :adjustable t :fill-pointer t)))
      (iter (for i from 0 to (- (fill-pointer vector) start-pointer 1))
	    (setf (aref new-vector i) (aref vector (+ start-pointer i))))
      (setf (fill-pointer new-vector) (- (fill-pointer vector) start-pointer)
	    start-pointer 0
	    vector new-vector))))
      
(defmethod buffer-push (elt (obj buffer-vector))
  (with-slots (vector) obj
    (vector-push-extend elt vector (array-dimension vector 0))))

(defmethod buffer-pop ((obj buffer-vector))
  (with-slots (vector start-pointer) obj
    (if (equal start-pointer (fill-pointer vector))
	(buffer-error "Attempt to pop from vector of zero (soft) length.")
	(vector-pop obj))))


;;; Pythonic approach to iteration
;;; iterators - classes with NEXT-ITER method, which raises STOP-ITERATION when iterator is depleted
(define-condition stop-iteration (error)
  ())
(defgeneric next-iter (iter)
  (:documentation "Main method of iteration protocol"))

(defclass string-iter ()
  ((pos :initform 0 :initarg :start)
   (str :initarg :string :initform "")))

(defclass stream-iter ()
  ((stream :initarg :stream :initform (error "STEAM is required argument"))))


(defun mk-string-iter (string &key (start 0))
  (make-instance 'string-iter :string string :start start))

(defun mk-stream-iter (stream)
  (make-instance 'stream-iter :stream stream))

(defmethod next-iter ((iter string-iter))
  (with-slots (str pos) iter
    (if (equal pos (length str))
	(error 'stop-iteration)
	(let ((char (char str pos)))
	  (incf pos)
	  char))))

(defmethod next-iter ((iter stream-iter))
  (with-slots (stream) iter
    (or (read-char stream nil nil nil)
	(error 'stop-iteration))))

(defclass cache-iterator ()
  ((cached-vals)
   (cached-pos :initform 0)
   (sub-iter :initform (error "Please, specify underlying iterator") :initarg :sub-iter)))

(defmethod initialize-instance :after ((this cache-iterator) &key &allow-other-keys)
  (with-slots (cached-vals) this
    (setf cached-vals (make-instance 'buffer-vector))
    (with-slots (vector) cached-vals
      (setf (fill-pointer vector) 0))))

(defun mk-cache-iter (sub-iter)
  (make-instance 'cache-iterator :sub-iter sub-iter))

(defun rewind (cache-iterator &optional new-pos)
  (with-slots (cached-vals cached-pos) cache-iterator
    (with-slots (vector start-pointer) cached-vals
      (when (null new-pos)
	(setf new-pos start-pointer))
      (cond ((< new-pos start-pointer)
	     (buffer-error "New position is less than (soft) beginning of the array."))
	    ((> new-pos (fill-pointer vector))
	     (buffer-error "New position is greater than cache range, and than read-from-stream value"))
	    (t (setf cached-pos new-pos))))))

(defun rel-rewind (cache-iterator &optional (delta 1))
  "Relative rewind"
  (with-slots (cached-vals cached-pos) cache-iterator
    (with-slots (vector start-pointer) cached-vals
      (cond ((< (- cached-pos delta) start-pointer)
	     (buffer-error "New position is less than (soft) beginning of the array."))
	    ((> (- cached-pos delta) (fill-pointer vector))
	     (buffer-error "New position is greater than cache range, and than read-from-stream value"))
	    (t (setf cached-pos (- cached-pos delta)))))))

(defun fast-forward (cache-iterator &optional (delta 1))
  "Relative fast-forward"
  (with-slots (cached-vals cached-pos) cache-iterator
    (with-slots (vector start-pointer) cached-vals
      (cond ((< (+ cached-pos delta) start-pointer)
	     (buffer-error "New position ~a is less than (soft) beginning of the array ~a."
			   (+ cached-pos delta) start-pointer))
	    ((> (+ cached-pos delta) (fill-pointer vector))
	     (buffer-error "New position ~a is greater than cache range, and than read-from-stream value ~a"
			   (+ cached-pos delta) (fill-pointer vector)))
	    (t (setf cached-pos (+ cached-pos delta)))))))

  
(defmethod next-iter ((iter cache-iterator))
  (with-slots (cached-vals cached-pos sub-iter) iter
    (with-slots (vector) cached-vals
      (if (equal cached-pos (fill-pointer vector))
	  (let ((new-val (next-iter sub-iter)))
	    (buffer-push new-val cached-vals)
	    (incf cached-pos)
	    new-val)
	  (let ((old-val (aref vector cached-pos)))
	    (incf cached-pos)
	    old-val)))))

(defmacro-driver (for var in-iter iter)
  (let ((kwd (if generate 'generate 'for)))
    (with-gensyms (g!-iter)
      `(progn (with ,g!-iter = ,iter)
	      (,kwd ,var next (let ((next-val (handler-case (next-iter ,g!-iter)
						(stop-iteration () (terminate)))))
				next-val))))))

(defgeneric start-of-iter-p (iter)
  (:documentation "T if the given iter is at the start. True by default."))
(defmethod start-of-iter-p ((iter t))
  t)

(defmethod start-of-iter-p ((iter cache-iterator))
  (with-slots (cached-pos cached-vals) iter
    (with-slots (start-pointer) cached-vals
      (equal start-pointer cached-pos))))

(defparameter the-iter nil)
(defparameter the-length 0)
(defparameter the-position 0)

(defmacro with-saved-iter-state ((iter) &body body)
  (with-gensyms (g!-cached-pos)
    `(let ((,g!-cached-pos (slot-value ,iter 'cached-pos)))
       (flet ((restore-iter-state ()
		(rewind ,iter ,g!-cached-pos)))
	 ,@body))))

(defun print-iter-state (&optional (cached-iter the-iter))
  (with-slots (cached-vals cached-pos) cached-iter
    (with-slots (start-pointer vector) cached-vals
      (if-debug "  p ~a s ~a f ~a P ~a L ~a" 
		cached-pos start-pointer (fill-pointer vector) the-position the-length))))

(defmethod hard-shrink ((obj cache-iterator) num-elts-discarded)
  (with-slots (cached-vals cached-pos) obj
    (decf cached-pos num-elts-discarded)
    (hard-shrink cached-vals num-elts-discarded)))
