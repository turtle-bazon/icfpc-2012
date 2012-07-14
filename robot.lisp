
(in-package :lambda-lifter)

(defun robot-coords (objects) (first (funcall objects :robot)))

(defmacro with-coords ((x y) coords &body body)
  (with-gensyms (my-coords)
    `(let* ((,my-coords ,coords)
            (,x (realpart ,my-coords))
            (,y (imagpart ,my-coords)))
       ,@body)))

(defmacro with-robot-coords ((x y) objects &body body)
  `(with-coords (,x ,y) (robot-coords ,objects)
     ,@body))

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
                   (t (funcall objects type))))
               (lambda () (cons ,path-symbol (funcall path)))
               metadata))))

(defrobot-move robot-move-left -1 0 :L)
(defrobot-move robot-move-right 1 0 :R)
(defrobot-move robot-move-up 0 1 :U)
(defrobot-move robot-move-down 0 -1 :D)

(defmacro defrobot-go-script (name delta-x delta-y mover &key push-check push-script)
  `(defun ,(form-symbol 'robot-go- name '-script) (world objects metadata)
     (with-robot-coords (rx ry) objects
       (let ((rx~ (+ rx ,delta-x)) (ry~ (+ ry ,delta-y)))
         (when (in-range-p metadata rx~ ry~)
           (ecase (funcall world rx~ ry~)
             (:robot (error "Something wrong: more than one robot encountered"))
             (:wall nil)
             (:rock ,(when (and push-check push-script)
                           `(when (,push-check world metadata rx~ ry~)
                              (list (function ,mover) (,push-script rx~ ry~)))))
             (:lambda (list (function ,mover) (lambda-collect rx~ ry~)))
             ((:open-lambda-lift :earth nil) (list (function ,mover)))))))))

(defrobot-go-script left -1 0 robot-move-left :push-check rock-can-be-pushed-left :push-script rock-push-left)
(defrobot-go-script right 1 0 robot-move-right :push-check rock-can-be-pushed-right :push-script rock-push-right)
(defrobot-go-script up 0 1 robot-move-up)
(defrobot-go-script down 0 -1 robot-move-up)
    
