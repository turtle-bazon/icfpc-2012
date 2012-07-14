
(in-package :lambda-lifter)

(defun water-update (world objects path metadata)
  (with-meta-bind (metadata water flooding)
    (let* ((water-level (+ water (floor (length path) flooding)))
	   (ry (imagpart (robot-coords objects))))
      (values world
	      (if (>= water-level ry)
		  (lambda (type)
		    (let ((prev-value (funcall objects type)))
		      (case type
			(:underwater (if prev-value
					 (+ prev-value 1)
					 0))
			(t prev-value))))
		  (lambda (type)
		    (case type
		      (:underwater nil)
		      (t (funcall objects type)))))
	      path
	      metadata))))
