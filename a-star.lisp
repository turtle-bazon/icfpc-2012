
(in-package :lambda-lifter)

(defun a*-search/accessible (start-x start-y target-x target-y accessible-p)
  (let ((open-list (make-hash-table))
        (closed-list (make-hash-table)))
    (setf (gethash (complex start-x start-y) open-list) t)
    (flet ((sq-dist (point-a point-b)
             (let ((x-diff (- (realpart point-a) (realpart point-b)))
                   (y-diff (- (imagpart point-a) (imagpart point-b))))
               (+ (* x-diff x-diff) (* y-diff y-diff))))
           (local-accessible-p (x y)
             (or (and (= x target-x) (= y target-y))
                 (funcall accessible-p x y))))
      (iter (for best-point = (iter (for (point v) in-hashtable open-list)
                                    (finding point minimizing (sq-dist point (complex target-x target-y)))))            
            (setf (gethash best-point closed-list) t)
            (remhash best-point open-list)
            (iter (for (dx dy) in '((-1 0) (1 0) (0 1) (0 -1)))
                  (for neighbour-x = (+ (realpart best-point) dx))
                  (for neighbour-y = (+ (imagpart best-point) dy))
                  (for neighbour = (complex neighbour-x neighbour-y))
                  (when (or (not (local-accessible-p neighbour-x neighbour-y))
                            (gethash neighbour closed-list))
                    (next-iteration))
                  (unless (gethash neighbour open-list)
                    (setf (gethash neighbour open-list) t)))
            (when (gethash (complex target-x target-y) open-list)
              (return-from a*-search/accessible t))
            (when (zerop (hash-table-count open-list))
              (return-from a*-search/accessible nil))))))
            
                  
                  
            
    

                  