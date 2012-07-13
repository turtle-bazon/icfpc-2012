
(in-package :lambda-lifter)

(defun robot-move-right (world objects)
  (let* ((robot-coords (first (funcall objects :robot)))
         (rx (realpart robot-coords))
         (ry (imagpart robot-coords)))
    (values (lambda (x y)
              (cond ((and (= x (1+ rx))
                          (= y ry))
                     :robot)
                    ((and (= x rx) (= y ry)) nil)
                    (t (funcall world x y))))
            (lambda (type)
              (case type
                (:robot (list (complex (1+ rx) ry)))
                (t (funcall objects type)))))))
                    

