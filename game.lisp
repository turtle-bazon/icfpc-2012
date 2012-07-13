
(in-package :lambda-lifter)

(defun find-closest-lambda (objects)
  (iter (with robot-coords = (first (funcall objects :robot)))
        (for lambda-coords in (funcall objects :lambda))
        (for x-diff = (- (realpart robot-coords) (realpart lambda-coords)))
        (for y-diff = (- (imagpart robot-coords) (imagpart lambda-coords)))
        (for sq-distance = (+ (* x-diff x-diff) (* y-diff y-diff)))
        (finding lambda-coords minimizing sq-distance)))

