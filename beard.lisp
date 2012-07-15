
(in-package :lambda-lifter)

(defun beard-growth (iworld imetadata bx by)
  (lambda (world objects path metadata)
    (let ((new-beards (iter outer
			(for y from (- by 1) to (+ by 1))
			(iter (for x from (- bx 1) to (+ bx 1))
			  (when (and (in-range-p imetadata x y)
				     (eq nil (funcall iworld x y)))
			    (in outer (collect (complex x y))))))))
      (values (lambda (x y)
		(cond ((find-if (lambda (coord)
				  (and (= x (realpart coord))
				       (= y (imagpart coord))))
				new-beards)
		       :beard)
		      (t (funcall world x y))))
	      (lambda (type)
		(case type
		  (:beard (append new-beards
				  (funcall objects type)))
		  (t (funcall objects type))))
	      path
	      metadata))))

(defun beards-growth (world objects path metadata)
  (with-meta-bind (metadata growth)
    (if growth
	(let ((current-grow (funcall objects :growth)))
	  (if (= current-grow 0)
	      (let ((world~ world)
		    (objects~ objects)
		    (path~ path)
		    (metadata~ metadata))
		(with-meta-bind (metadata width)
		  (iter (for beard-coord in (sort (copy-list (funcall objects :beard)) #'<
						  :key (lambda (c) (+ (realpart c) (* (imagpart c) width)))))
		    (multiple-value-setq (world~ objects~ path~ metadata~)
		      (funcall (beard-growth world metadata (realpart beard-coord) (imagpart beard-coord))
			       world~ objects~ path~ metadata~))))
		(values world~
			(lambda (type)
			  (case type
			    (:beard (remove-duplicates (funcall objects~ type)))
			    (:growth (- growth 1))
			    (t (funcall objects~ type))))
			path~
			metadata~))
	      (values world
		      (lambda (type)
			(case type
			  (:growth (- current-grow 1))
			  (t (funcall objects type))))
		      path
		      metadata)))
	(values world objects path metadata))))

