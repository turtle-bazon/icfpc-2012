
(in-package :lambda-lifter)

(defun in-range-p (metadata x y)
  (with-meta-bind (metadata width height)
    (and (>= x 1)
	 (>= y 1)
	 (<= x width)
	 (<= y height))))

(defun rock-fall (world objects path metadata rock-coords)
  (let* ((rx (realpart rock-coords))
	 (ry (imagpart rock-coords))
	 (rx-under rx)
	 (ry-under (- ry 1)))
    (when (and (in-range-p metadata rx-under ry-under)
	       (eq nil (funcall world rx-under ry-under)))
      (values (lambda (x y)
		(cond ((and (= x rx)
			    (= y ry))
		       nil)
		      ((and (= x rx-under)
			    (= y ry-under))
		       :rock)))
	      (lambda (type)
		(case type
		  (:rock (cons (complex rx-under ry-under)
			       (remove (complex rx ry) (funcall objects :rock))))
		  (t (funcall objects type))))
	      path
	      metadata))))

(defun rock-move (world objects path metadata rock-coords)
  (or (rock-fall world objects path metadata rock-coords)))
