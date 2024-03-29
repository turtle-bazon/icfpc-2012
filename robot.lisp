
(in-package :lambda-lifter)

(defmacro defrobot-move (name delta-x delta-y path-symbol)
  `(defun ,name (world objects path metadata)
     (with-robot-coords (rx ry) objects
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
	       metadata))))

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
	  (let ((adjacent-beards-coords (iter outer
					  (for y from (- ry 1) to (+ ry 1))
					  (iter (for x from (- rx 1) to (+ rx 1))
					    (when (and (in-range-p metadata x y)
						       (eq :beard (funcall world x y)))
					      (in outer (collect (complex x y))))))))
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

(defmacro defrobot-go-script (name delta-x delta-y path-symbol mover &key push-check push-script)
  `(defun ,(form-symbol 'robot-go- name '-script) (world objects path metadata)
     (declare (ignorable world objects path metadata))
     (with-robot-coords (rx ry) objects
       (let ((rx~ (+ rx ,delta-x)) (ry~ (+ ry ,delta-y)))
         (when (in-range-p metadata rx~ ry~)
           (ecase (funcall world rx~ ry~)
             ((:wall :closed-lambda-lift :target-1 :target-2 :target-3 :target-4 :target-5 :target-6 :target-7 :target-8 :target-9 :beard) nil)
             ((:portal-a :portal-b :portal-c :portal-d :portal-e :portal-f :portal-g :portal-h :portal-i)
              (list (step-into-portal rx~ ry~ ,path-symbol)))
             ((:rock :horock) ,(when (and push-check push-script)
                                     `(when (,push-check world metadata rx~ ry~)
                                        (list (,push-script rx~ ry~) (function ,mover)))))
             (:lambda (list (function ,mover) (collect-lambda/open-lift rx~ ry~)))
             (:razor (list (function ,mover) (collect-razor rx~ ry~)))
             (:open-lambda-lift (list (function ,mover) (collect-lift rx~ ry~)))
             ((:robot :earth nil) (list (function ,mover)))))))))

(defrobot-go-script left -1 0 :L robot-move-left :push-check rock-can-be-pushed-left :push-script rock-push-left)
(defrobot-go-script right 1 0 :R robot-move-right :push-check rock-can-be-pushed-right :push-script rock-push-right)
(defrobot-go-script up 0 1 :U robot-move-up)
(defrobot-go-script down 0 -1 :D robot-move-down)

(defun robot-go-wait-script (world objects path metadata)
  (declare (ignore world path objects metadata))
  (list #'robot-wait))

(defun robot-go-razor-script (world objects path metadata)
  (declare (ignore world path metadata))
  (with-robot-coords (rx ry)
      objects
    (symbol-macrolet ((razors-count (funcall objects :razors))
                      (beards-around
                       (remove-if-not
                        (lambda (coord)
                          (with-coords (bx by)
                              coord
                            (and (>= bx (- rx 1)) (<= bx (+ rx 1))
                                 (>= by (- ry 1)) (<= by (+ ry 1)))))
                        (funcall objects :beard))))
      (when (and (> razors-count 0) beards-around)
        (list #'robot-apply-razor)))))

