
(in-package :esrap-liquid)

(cl-itertools:defiter %mk-tokenizer (expression token-iter &key junk-allowed)
  (let ((the-iter token-iter)
	(*cache* (make-cache)))
    (iter (while t)
	  (setf the-position 0
		the-length 0
		max-failed-position 0
		max-rule-stack nil
		max-message ""
		positive-mood t)
	  (tracing-init
	    (with-tmp-rule (tmp-rule expression)
	      (let ((result (handler-case (descend-with-rule tmp-rule)
			      (internal-esrap-error ()
				(if junk-allowed
				    (values nil 0)
				    (simple-esrap-error
				     (iter-last-text max-failed-position)
				     max-rule-stack max-failed-position max-message))))))
		(cl-itertools:yield (values result the-length)))))
	  (hard-shrink *cache* the-length)
	  (hard-shrink the-iter the-length))))
