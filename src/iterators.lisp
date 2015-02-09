
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

(defclass super-cache-iterator ()
  ((cached-vals)))

(defmethod initialize-instance :after ((this super-cache-iterator) &key &allow-other-keys)
  (with-slots (cached-vals) this
    (setf cached-vals (make-array 1))))