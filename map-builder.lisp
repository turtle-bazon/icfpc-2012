
(in-package :lambda-lifter)

(defmacro with-meta-bind ((metadata &rest vars) &body body)
  `(let (,@(iter (for var in vars)
                 (collect `(,var (second (assoc ,(form-keyword var) ,metadata))))))
     ,@body))

(defun make-mine (stream)
  (let ((objects (make-hash-table :test 'eq)))
    (let* ((metadata (apply-map-parser stream
                                       (lambda (type width x y)
                                         (push (+ (* (1- y) width) (1- x))
                                               (gethash type objects))))))
      (with-meta-bind (metadata width height)
        (assert (and width height) nil "Either width or height in metadata not found")
        (let ((world (make-array (* width height))))
          (iter (for (type objects-list) in-hashtable objects)
                (iter (for coord in objects-list)
                      (setf (elt world coord) type)
                      (for parsed-coord = (complex (1+ (mod coord width))
                                                   (- height (truncate coord width))))
                      (collect parsed-coord into parsed-coords)
                      (finally (setf (gethash type objects) parsed-coords))))
          (values
           (lambda (x y)
             (assert (and (<= 1 x width) (<= 1 y height)))
             (elt world (+ (* (- height y) width) (1- x))))
           (lambda (type)
             (gethash type objects))
           (lambda () nil)
           metadata))))))

(defun apply-map-parser (stream cell-receiver)
  (iter outer
        (with state = :reading-map)
        (with max-cell-index = nil)
        (for line in-stream stream using #'read-line)
        (for rev-row-index from 1)
        (when (zerop (length line))
          (setf state :reading-weather)
          (next-iteration))
        (ecase state
          (:reading-map
           (maximizing rev-row-index into max-row-index)
           (unless max-cell-index
             (setf max-cell-index (length line)))
           (iter (for char in-string line)
                 (for cell-index from 1)
                 (assert (<= cell-index max-cell-index))
                 (unless (char= char #\Space)
                   (funcall cell-receiver
                            (ecase char
                              (#\R :robot)
                              (#\# :wall)
                              (#\* :rock)
                              (#\\ :lambda)
                              (#\L :closed-lambda-lift)
                              (#\. :earth))
                            max-cell-index
                            cell-index
                            rev-row-index))))
          (:reading-weather
           (for (meta value-string) = (split-sequence:split-sequence #\Space line))
           (collect (list (form-keyword (string-upcase meta))
                          (parse-integer value-string))
             into metadata)))
        (finally
         (return-from outer (append (list (list :width max-cell-index)
                                          (list :height max-row-index))
                                    metadata)))))

