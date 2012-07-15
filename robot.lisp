
(in-package :lambda-lifter)

(defmacro defrobot-move (name delta-x delta-y path-symbol)
  `(defun ,name (world objects path metadata)
     (with-robot-coords (rx ry) objects
       (let ((new-x (+ rx ,delta-x))
	     (new-y (+ ry ,delta-y)))
	 (if (member (funcall world new-x new-y) '(:portal-a :portal-b :portal-c :portal-d :portal-e :portal-f :portal-g :portal-h :portal-i))
	     (let* ((portal (funcall world new-x new-y))
		    (target (second (meta-value metadata portal)))
		    (target-coords (first (funcall objects target)))
		    (affected-portals (mapcar (lambda (portal-meta) (car portal-meta))
					      (remove-if-not
					       (lambda (meta)
						 (and (= (length meta) 2)
						      (eq (car (cdr meta)) target)))
					       metadata)))
		    (affected-portals-coords (mapcar (lambda (aff-portal)
						       (car (funcall objects aff-portal))) affected-portals)))
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
		       (lambda () (cons ,path-symbol (funcall path)))
		       metadata))
	     (values (lambda (x y)
		       (cond ((and (= x (+ rx ,delta-x))
				   (= y (+ ry ,delta-y)))
			      :robot)
			     ((and (= x rx) (= y ry)) nil)
			     (t (funcall world x y))))
		     (lambda (type)
		       (case type
			 (:robot (list (complex (+ rx ,delta-x) (+ ry ,delta-y))))
			 (:earth (remove (complex rx ry) (funcall objects type)))
			 (t (funcall objects type))))
		     (lambda () (cons ,path-symbol (funcall path)))
		     metadata))))))

(defrobot-move robot-move-left -1 0 :L)
(defrobot-move robot-move-right 1 0 :R)
(defrobot-move robot-move-up 0 1 :U)
(defrobot-move robot-move-down 0 -1 :D)

(defun robot-wait (world objects path metadata)
  (values world objects (lambda () (cons :W (funcall path))) metadata))

(defun robot-apply-razor (world objects path metadata)
  (let ((razors-count (funcall objects :razors)))
    (if (plusp razors-count)
	(with-robot-coords (rx ry) objects
	  (let ((adjacent-beards-coords (iter (for y from (- ry 1) to (+ ry 1))
					  (iter (for x from (- rx 1) to (+ rx 1))
					    (when (and (in-range-p metadata x y)
						       (eq :beard (funcall world x y)))
					      (collect (complex x y)))))))
	    (values (lambda (x y)
		      (cond ((find-if (lambda (coord)
					(and (= x (realpart coord))
					     (= y (imagpart coord))))
				      adjacent-beards-coords)
			     nil)
			    (t (funcall world x y))))
		    (lambda (type)
		      (case type
			(:beard (remove-if (lambda (coord)
					     (member coord adjacent-beards-coords))
					   (funcall objects type)))
			(:razors (- (funcall objects type) 1))
			(t (funcall objects type))))
		    (lambda () (cons :S (funcall path)))
		    metadata)))
	(values world objects (lambda () (cons :S (funcall path))) metadata))))

(defmacro defrobot-go-script (name delta-x delta-y mover &key push-check push-script)
  `(defun ,(form-symbol 'robot-go- name '-script) (world objects path metadata)
     (declare (ignorable world objects path metadata))
     (with-robot-coords (rx ry) objects
       (let ((rx~ (+ rx ,delta-x)) (ry~ (+ ry ,delta-y)))
         (when (in-range-p metadata rx~ ry~)
           (ecase (funcall world rx~ ry~)
             ((:wall :closed-lambda-lift :target-1 :target-2 :target-3 :target-4 :target-5 :target-6 :target-7 :target-8 :target-9) nil)
             (:rock ,(when (and push-check push-script)
                           `(when (,push-check world metadata rx~ ry~)
                              (list (function ,mover) (,push-script rx~ ry~)))))
             (:lambda (list (function ,mover) (collect-lambda/open-lift rx~ ry~) (function path-set-cleared)))
             (:open-lambda-lift (list (function ,mover) (collect-lift rx~ ry~) (function path-set-cleared)))
             ((:robot :earth nil) (list (function ,mover)))))))))


(defrobot-go-script left -1 0 robot-move-left :push-check rock-can-be-pushed-left :push-script rock-push-left)
(defrobot-go-script right 1 0 robot-move-right :push-check rock-can-be-pushed-right :push-script rock-push-right)
(defrobot-go-script up 0 1 robot-move-up)
(defrobot-go-script down 0 -1 robot-move-down)
(defrobot-go-script wait 0 0 robot-wait)
(defrobot-go-script razor 0 0 robot-apply-razor)

