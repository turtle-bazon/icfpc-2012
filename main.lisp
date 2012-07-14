
(in-package :lambda-lifter)

(defun game-loop ()
  nil)

(defun main ()
  (in-package :lambda-lifter)
  (sb-sys:enable-interrupt
   sb-unix:sigint
   (lambda (&rest args)
     (declare (ignore args))
     (sb-sys:invoke-interruption
      (lambda ()
	(sb-sys:with-interrupts
	  (setf *force-shutdown-p* t))))))
  (game-loop))

