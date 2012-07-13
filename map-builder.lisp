
(in-package :lambda-lifter)

(defun apply-map-parser (stream cell-receiver)
  (iter (for line in-stream stream using #'read-line)
        (for row-index from 1)
        (iter (for char in-string line)
              (for cell-index from 1)
              (maximizing cell-index into max-cell-index)
              (assert (and max-cell-index (<= cell-index max-cell-index)))
              (funcall cell-receiver
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


