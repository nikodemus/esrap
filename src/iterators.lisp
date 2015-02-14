
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


(defmethod soft-shrink ((obj buffer-vector) (num-elts-discarded integer))
  (with-slots (vector start-pointer) obj
    (let ((fill-pointer (fill-pointer vector)))
      (if (> (+ start-pointer num-elts-discarded) fill-pointer)
	  (error "Attempt to soft-shrink buffer more than its active length")
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
	(error "Attempt to pop from vector of zero (soft) length.")
	(vector-pop obj))))



(defclass super-cache-iterator ()
  ((cached-vals)))

(defmethod initialize-instance :after ((this super-cache-iterator) &key &allow-other-keys)
  (with-slots (cached-vals) this
    (setf cached-vals (make-array 1))))