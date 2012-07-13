
(in-package :lambda-lifter)

(defmacro defrobot-move (name delta-x delta-y)
  `(defun ,name (world objects)
     (let* ((robot-coords (first (funcall objects :robot)))
            (rx (realpart robot-coords))
            (ry (imagpart robot-coords)))
       (values (lambda (x y)
                 (cond ((and (= x (+ rx ,delta-x))
                             (= y (+ ry ,delta-y)))
                        :robot)
                       ((and (= x rx) (= y ry)) nil)
                       (t (funcall world x y))))
               (lambda (type)
                 (case type
                   (:robot (list (complex (+ rx ,delta-x) (+ ry ,delta-y))))
                   (t (funcall objects type))))))))

(defrobot-move robot-move-left -1 0)
(defrobot-move robot-move-right 1 0)
(defrobot-move robot-move-up 0 1)
(defrobot-move robot-move-down 0 -1)

