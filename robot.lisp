
(in-package :lambda-lifter)

(defmacro defrobot-move (name delta-x delta-y path-symbol)
  `(defun ,name (world objects path)
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
                   (t (funcall objects type))))
               (lambda () (cons ,path-symbol (funcall path)))))))

(defrobot-move robot-move-left -1 0 :L)
(defrobot-move robot-move-right 1 0 :R)
(defrobot-move robot-move-up 0 1 :U)
(defrobot-move robot-move-down 0 -1 :D)

