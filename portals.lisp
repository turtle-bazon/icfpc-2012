
(in-package :lambda-lifter)

(defun step-into-portal (px py path-symbol)
  (lambda (world objects path metadata)
    (let* ((portal (funcall world px py))
	   (target (second (meta-value metadata portal)))
	   (target-coords (first (funcall objects target)))
	   (affected-portals (mapcar (lambda (portal-meta)
				       (car portal-meta))
				     (remove-if-not
				      (lambda (meta)
					(and (= (length meta) 2)
					     (eq (car (cdr meta)) target)))
				      metadata)))
	   (affected-portals-coords (mapcar (lambda (aff-portal)
					      (car (funcall objects aff-portal))) affected-portals)))
      (with-robot-coords (rx ry) objects
	(print portal)
	(print target)
	(print target-coords)
	(values (lambda (x y)
		  (cond ((find-if (lambda (coord)
				    (and (= x (realpart coord))
					 (= y (imagpart coord))))
				  affected-portals-coords)
			 nil)
			((and (= x rx) (= y ry)) nil)
			((and (= x (realpart target-coords))
			      (= y (imagpart target-coords)))
			 :robot)
			(t (funcall world x y))))
		(lambda (type)
		  (cond ((eq type :robot) (list (complex (realpart target-coords)
							 (imagpart target-coords))))
			((eq type target) nil)
			((member type affected-portals) nil)
			(t (funcall objects type))))
		(lambda () (cons path-symbol (funcall path)))
		metadata)))))

