
(in-package :lambda-lifter)

(defparameter *map-defaults*
  '((:WATER . 0)
    (:FLOODING . 0)
    (:WATERPROOF . 10)
    (:GROWTH . 25)
    (:RAZORS . 0)))

(defmacro with-meta-bind ((metadata &rest vars) &body body)
  `(let (,@(iter (for var in vars)
                 (collect `(,var (second (assoc ,(form-keyword var) ,metadata))))))
     ,@body))

(defun meta-value (metadata key)
  (find-if (lambda (meta) (eq (car meta) key)) metadata))

(defun map-has-flooding-p (world objects metadata)
  (declare (ignore world objects metadata))
  t)

(defun map-has-portals-p (world objects metadata)
  (declare (ignore world objects metadata))
  t)

(defun map-has-beards-p (world objects metadata)
  (declare (ignore world objects metadata))
  t)

(defun map-has-horocks-p (world objects metadata)
  (declare (ignore world objects metadata))
  t)

(defun make-mine (stream)
  (let ((objects (make-hash-table :test 'eq)))
    (let* ((metadata (apply-map-parser stream
                                       (lambda (type width x y)
                                         (push (+ (* (1- y) width) (1- x))
                                               (gethash type objects))))))
      (with-meta-bind (metadata width height growth razors)
        (assert (and width height) nil "Either width or height in metadata not found")
        (let ((world (make-array (* width height) :initial-element nil)))
          (iter (for (type objects-list) in-hashtable objects)
                (iter (for coord in objects-list)
                      (setf (elt world coord) type)
                      (for parsed-coord = (complex (1+ (mod coord width))
                                                   (- height (truncate coord width))))
                      (collect parsed-coord into parsed-coords)
                      (finally (setf (gethash type objects) parsed-coords))))
	  (setf (gethash :razors objects) razors)
	  (setf (gethash :growth objects) (when growth (- growth 1)))
          (values
           (lambda (x y)
             (assert (and (<= 1 x width) (<= 1 y height)))
             (elt world (+ (* (- height y) width) (1- x))))
           (lambda (type)
	     (case type
	       (t (gethash type objects))))
           (lambda () nil)
           metadata))))))

(defun apply-map-parser (stream cell-receiver)
  (multiple-value-bind (map width height)
      (iter
	(with state = :reading-map)
	(with map-width = 0)
	(with map-height = 0)
	(for line in-stream stream using #'read-line)
	(when (zerop (length line))
	  (setf state nil))
	(when (eq state :reading-map)
	  (setf map-width (max map-width (length line)))
	  (incf map-height))
	(collect line into lines)
	(finally (return (values lines map-width map-height))))
    (iter outer
      (with state = :reading-map)
      (for line in map)
      (for rev-row-index from 1)
      (when (zerop (length line))
	(setf state :reading-metadata)
	(next-iteration))
      (ecase state
	(:reading-map
	 (iter (for char in-string line)
	   (for cell-index from 1)
	   (assert (<= cell-index width))
	   (unless (char= char #\Space)
	     (funcall cell-receiver
		      (ecase char
			(#\R :robot)
			(#\# :wall)
			(#\* :rock)
			(#\\ :lambda)
			(#\L :closed-lambda-lift)
			(#\O :open-lambda-lift)
			(#\. :earth)
			(#\A :portal-a)
			(#\B :portal-b)
			(#\C :portal-c)
			(#\D :portal-d)
			(#\E :portal-e)
			(#\F :portal-f)
			(#\G :portal-g)
			(#\H :portal-h)
			(#\I :portal-i)
			(#\1 :target-1)
			(#\2 :target-2)
			(#\3 :target-3)
			(#\4 :target-4)
			(#\5 :target-5)
			(#\6 :target-6)
			(#\7 :target-7)
			(#\8 :target-8)
			(#\9 :target-9)
			(#\W :beard)
			(#\! :razor)
			(#\@ :horock))
		      width
		      cell-index
		      rev-row-index))))
	(:reading-metadata
	 (for metasplit = (split-sequence:split-sequence #\Space line))
	 (let ((pname (form-keyword (string-upcase (first metasplit)))))
	   (collect (case pname
		      (:TRAMPOLINE (list (form-keyword (format nil "PORTAL-~a" (string-upcase (second metasplit))))
					 (form-keyword (format nil "TARGET-~a" (fourth metasplit)))))
		      (t (list pname (parse-integer (second metasplit)))))
	     into metadata))))
      (finally
       (return-from outer (append (list (list :best 0 nil)
					(list :width width)
					(list :height height))
				  metadata
				  (iter (for (param . default-value) in *map-defaults*)
				    (unless (assoc param metadata)
				      (collect (list param default-value))))))))))

