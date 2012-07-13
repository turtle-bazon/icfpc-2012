
(in-package :lambda-lifter)

(defun array-map-builder ()
  (let ((initial-world (make-array '(0 0) :adjustable t))
	(robot-coords '(nil nil)))
    (lambda (command &optional type x y)
      (ecase command
	(:build (destructuring-bind (x-size y-size)
		    (array-dimensions initial-world)
		  (when (or (> x x-size )
			    (> y y-size))
		    (adjust-array initial-world (list (max x x-size) (max y y-size))))
		  (setf (aref initial-world (- x 1) (- y 1)) type)
		  (when (eq type :robot)
		    (setf robot-coords (list x y)))))
	(:receive (lambda (command x y)
		    (ecase command
		      (:get (aref initial-world (- x 1) (- y 1)))
		      (:dimensions (array-dimensions initial-world))
		      (:fallback (error "No more fallbacks")))))))))

(defun apply-map-parser (stream cell-receiver)
  (iter outer
        (with state = :reading-map)
        (for line in-stream stream using #'read-line)
        (for rev-row-index from 1)
        (when (zerop (length line))
          (setf state :reading-weather)
          (next-iteration))
        (ecase state
          (:reading-map
           (maximizing rev-row-index into max-row-index)
           (iter (for char in-string line)
                 (for cell-index from 1)
                 (in outer (maximizing cell-index into max-cell-index))
                 (assert (and max-cell-index (<= cell-index max-cell-index)))
                 (funcall cell-receiver
                          :build
                          (ecase char
                            (#\R :robot)
                            (#\# :wall)
                            (#\* :rock)
                            (#\\ :lambda)
                            (#\L :closed-lambda-lift)
                            (#\. :earth)
                            (#\Space :empty))
                          cell-index
                          rev-row-index)))
          (:reading-weather
           (for (meta value-string) = (split-sequence:split-sequence #\Space line))
           (collect (list (form-keyword (string-upcase meta))
                          (parse-integer value-string))
             into metadata)))
        (finally
         (return-from outer (append (list (list :width max-cell-index)
                                          (list :height max-row-index))
                                    metadata)))))


(defun load-map (file)
  (let ((builder (array-map-builder)))
    (with-open-file (s file)
      (apply-map-parser s builder)
      (funcall builder :receive))))