
(in-package :lambda-lifter)

(defun a*-search/accessible-group (start-x start-y targets accessible-p)
  (declare (optimize (speed 3))
           (type fixnum start-x start-y)
           (type list targets)
           (type function accessible-p))
  (let ((open-list (make-hash-table))
	(closed-list (make-hash-table)))
    (setf (gethash (complex start-x start-y) open-list) t)
    (flet ((sq-dist (point-a point-b)
             (declare (type (complex fixnum) point-a point-b))
	     (let ((x-diff (- (realpart point-a) (realpart point-b)))
		   (y-diff (- (imagpart point-a) (imagpart point-b))))
               (declare (type fixnum x-diff y-diff))
	       (+ (* x-diff x-diff) (* y-diff y-diff))))
	   (local-accessible-p (x y target-x target-y)
             (declare (type fixnum x y target-x target-y))
	     (or (and (= x target-x) (= y target-y))
		 (funcall accessible-p x y))))
      (iter main
            (for best-points = (remove nil
                                       (remove-duplicates
                                        (iter (for (target-x target-y) in targets)
                                              (collect
                                                  (iter (for (point v) in-hashtable open-list)
                                                        (finding point minimizing (sq-dist point (complex target-x target-y)))))))))
            (while best-points)
            (iter (for best-point in best-points)
                  (setf (gethash best-point closed-list) t)
                  (remhash best-point open-list))
            (iter
              (for neighbour in (remove-duplicates
                                 (iter outer
                                       (for (dx dy) in '((-1 0) (1 0) (0 1) (0 -1)))
                                       (iter (for best-point in best-points)
                                             (in outer (collect (complex (+ (realpart best-point) dx)
                                                                         (+ (imagpart best-point) dy))))))))
              (for neighbour-x = (realpart neighbour))
              (for neighbour-y = (imagpart neighbour))
              (when (or (gethash neighbour closed-list)
                        (reduce
                         (lambda (x1 x2)
                           (or x1 x2))
                         (mapcar
                          (lambda (target)
                            (destructuring-bind (target-x target-y)
                                target
                              (not (local-accessible-p neighbour-x neighbour-y
                                                       target-x target-y))))
                          targets)))
                (setf (gethash neighbour closed-list) t)
                (next-iteration))
              (unless (gethash neighbour open-list)
                (setf (gethash neighbour open-list) t)))))
    (iter (for (target-x target-y) in targets)
          (if (gethash (complex target-x target-y) closed-list)
              (collect t)
              (collect nil)))))

