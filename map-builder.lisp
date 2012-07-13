
(in-package :lambda-lifter)

(defun array-map-builder ()
  (let ((initial-world (make-array '(0 0) :adjustable t)))
    (lambda (command &optional type x y)
      (ecase command
	(:build (destructuring-bind (x-size y-size)
		    (array-dimensions initial-world)
		  (when (or (> x x-size )
			    (> y y-size))
		    (adjust-array initial-world (list (max y y-size) (max x x-size))))
		  (setf (aref initial-world (- y 1) (- x 1)) type)))
	(:receive initial-world)))))

(defun apply-map-parser (stream cell-receiver)
  (iter (for line in-stream stream using #'read-line)
        (for row-index from 1)
        (iter (for char in-string line)
              (for cell-index from 1)
              (maximizing cell-index into max-cell-index)
              (assert (and max-cell-index (<= cell-index max-cell-index)))
              (funcall cell-receiver :build
                       (ecase char
                         (#\R :robot)
                         (#\# :wall)
                         (#\* :rock)
                         (#\\ :lambda)
                         (#\L :closed-lambda-lift)
                         (#\. :earth)
                         (#\Space :empty))
                       cell-index
                       row-index))))

(defun load-map (file)
  (let ((builder (array-map-builder)))
    (with-open-file (s file)
      (apply-map-parser s builder)
      (funcall builder :receive))))