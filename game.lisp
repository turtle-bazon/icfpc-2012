
(in-package :lambda-lifter)

(defun find-closest-lambda (objects)
  (with-robot-coords (rx ry) objects
    (iter (for lambda-coords in (funcall objects :lambda))
          (for x-diff = (- rx (realpart lambda-coords)))
          (for y-diff = (- ry (imagpart lambda-coords)))
          (for sq-distance = (+ (* x-diff x-diff) (* y-diff y-diff)))
          (finding lambda-coords minimizing sq-distance))))



