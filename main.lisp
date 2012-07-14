
(in-package :lambda-lifter)

(defun main (args)
  (declare (ignore args))
  (in-package :lambda-lifter)
  (sb-sys:enable-interrupt
   sb-unix:sigint
   (lambda (&rest args)
     (declare (ignore args))
     (sb-sys:invoke-interruption
      (lambda ()
	(sb-sys:with-interrupts
	  (setf *force-shutdown-p* t))))))
  (multiple-value-call #'solve-world (make-mine *standard-input*)))
