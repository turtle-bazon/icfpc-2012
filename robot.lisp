
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

(defmacro defrobot-go-script (name delta-x delta-y mover &key push-check push-script step-portal-script)
  `(defun ,(form-symbol 'robot-go- name '-script) (world objects metadata)
     (with-robot-coords (rx ry) objects
       (let ((rx~ (+ rx ,delta-x)) (ry~ (+ ry ,delta-y)))
         (when (in-range-p metadata rx~ ry~)
           (ecase (funcall world rx~ ry~)
             ((:wall :closed-lambda-lift :target-1 :target-2 :target-3 :target-4 :target-5 :target-6 :target-7 :target-8 :target-9) nil)
	     ((:portal-a :portal-b :portal-c :portal-d :portal-e :portal-f :portal-g :portal-h :portal-i)
	      (list (function ,mover) (,step-portal-script rx~ ry~)))
             (:rock ,(when (and push-check push-script)
                           `(when (,push-check world metadata rx~ ry~)
                              (list (function ,mover) (,push-script rx~ ry~)))))
             (:lambda (list (function ,mover) (collect-lambda/open-lift rx~ ry~) (function path-set-cleared)))
             (:open-lambda-lift (list (function ,mover) (collect-lift rx~ ry~) (function path-set-cleared)))
             ((:robot :earth nil) (list (function ,mover)))))))))

(defrobot-go-script left -1 0 robot-move-left :push-check rock-can-be-pushed-left :push-script rock-push-left :step-portal-script step-into-portal)
(defrobot-go-script right 1 0 robot-move-right :push-check rock-can-be-pushed-right :push-script rock-push-right :step-portal-script step-into-portal)
(defrobot-go-script up 0 1 robot-move-up :step-portal-script step-into-portal)
(defrobot-go-script down 0 -1 robot-move-down :step-portal-script step-into-portal)
(defrobot-go-script wait 0 0 robot-wait)


