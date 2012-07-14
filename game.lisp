
(in-package :lambda-lifter)

(defun estimate-importance (rx ry ox oy width height)
  (+ (* oy (+ (* width width) (* height height)))
     (let ((x-diff (- rx ox))
           (y-diff (- ry oy)))
       (+ (* x-diff x-diff) (* y-diff y-diff)))))

(defun exists-path-to-p (rx ry ox oy world path metadata)
  (labels ((validate-around (x y history)
             (let ((history (cons (complex x y) history)))
               (or (validate-rec (1- x) y history)
                   (validate-rec (1+ x) y history)
                   (validate-rec x (1+ y) history)
                   (validate-rec x (1- y) history))))
           (validate-rec (x y history)
             (unless (or (position (complex x y) history :test #'eql)
                         (visited-p (- x rx) (- y ry) path)
                         (not (in-range-p metadata x y)))
               (case (funcall world x y)
                 (:robot t)
                 ((:wall :rock :lambda :closed-lambda-lift :open-lambda-lift) nil)
                 (t (validate-around x y history))))))
    (validate-around ox oy '())))

(defun find-most-important-object (type world objects path metadata)
  (with-robot-coords (rx ry) objects
    (with-meta-bind (metadata width height)
      (iter (for coords in (funcall objects type))
            (with-coords (ox oy) coords
              (when (exists-path-to-p rx ry ox oy world path metadata)
                (finding coords minimizing (estimate-importance rx ry ox oy width height))))))))

(defun choose-target (world objects path metadata)
  (iter (for possible-targets in '(:lambda :open-lambda-lift))
        (for nearest-object = (find-most-important-object possible-targets world objects path metadata))
        (when nearest-object
          (return-from choose-target nearest-object))))

(defun robot-ai (world objects path metadata)
  (let ((current-target (choose-target world objects path metadata)))
    (unless current-target
      (return-from robot-ai nil))
    (with-coords (target-x target-y) current-target
      (with-robot-coords (rx ry) objects
        (iter (for (dx dy script-builder)
                   in (sort (list (list -1 0 #'robot-go-left-script)
                                  (list 1 0 #'robot-go-right-script)
                                  (list 0 1 #'robot-go-up-script)
                                  (list 0 -1 #'robot-go-down-script)
                                  (list 0 0 #'robot-go-wait-script))
                            #'<
                            :key (lambda (entry)
                                   (let* ((erx (+ rx (first entry)))
                                          (ery (+ ry (second entry)))
                                          (x-diff (- target-x erx))
                                          (y-diff (- target-y ery)))
                                     (* (+ (* x-diff x-diff) (* y-diff y-diff))
                                        (case (funcall world erx ery)
                                          (nil 1.0)
                                          (:earth 1.5)
                                          (:lambda 2.0)
                                          (t 3.0)))))))
              (unless (visited-p dx dy path)
                (for script = (funcall script-builder world objects metadata))
                (when script
                  (collect script))))))))

(defun make-script (script)
  (lambda (world objects path metadata)
    (iter (for action in script)
          (multiple-value-setq (world objects path metadata)
            (funcall action world objects path metadata))
          (finally (return (values world objects path metadata))))))

(defun update-hiscore (world objects path metadata)
  (let ((current-score (score world objects path metadata)))
    (when current-score
      (let ((best (assoc :best metadata)))
        (when (and best (> current-score (second best)))
          (setf (second best) current-score
                (third best) path)))
      current-score)))

(defun game-loop (world objects path metadata)

  ;; (declare (optimize (debug 3)))
  ;; (dump-world world objects path metadata)
  ;; (format t "Target: ~a; score: ~a; path: ~a~%" (choose-target world objects path metadata) (score world objects path metadata) (dump-path nil path))
  ;; ;;(sleep 0.2)
  ;; (break)
  
  (let ((current-score (update-hiscore world objects path metadata)))    
    ;; check for extremal condition
    (when (or (not current-score) *force-shutdown-p*)
      (let ((path (lambda () (cons :A (funcall path)))))
        (update-hiscore world objects path metadata)
        (return-from game-loop
          (values world objects path metadata))))
    ;; run robot ai and perform the game turn
    (iter (for robot-step-script in (robot-ai world objects path metadata))
          (unless robot-step-script
            (return-from game-loop
              (values world objects path metadata)))
          (for world-script = (make-script (append robot-step-script
                                                   (list #'rocks-move
                                                         #'water-update
                                                         #'game-loop))))
          (funcall world-script world objects path metadata)))
  (values world objects path metadata))

(defun solve-world (world objects path metadata)
  (game-loop world objects path metadata)
  (let ((best-solve (third (assoc :best metadata))))
    (dump-path t best-solve)))

;; Debugging stuff

(defun dump-world (world objects path metadata)
  (declare (ignorable world objects path metadata))
  (with-meta-bind (metadata width height water flooding)
    (iter
      (with water-level = (+ water (floor (path-length path) flooding)))
      (for y from height downto 1)
      (format t "~a" (if (<= y water-level) "W" " "))
      (iter (for x from 1 to width)
	(format t "~a" (case (funcall world x y)
			 (:lambda #\\)
			 (:robot #\R)
			 (:rock #\*)
			 (:wall #\#)
			 (:earth #\.)
			 (:open-lambda-lift #\O)
			 (:closed-lambda-lift #\L)
			 (:portal-a #\A)
			 (:portal-b #\B)
			 (:portal-c #\C)
			 (:portal-d #\D)
			 (:portal-e #\E)
			 (:portal-f #\F)
			 (:portal-g #\G)
			 (:portal-h #\H)
			 (:portal-i #\I)
			 (:target-1 #\1)
			 (:target-2 #\2)
			 (:target-3 #\3)
			 (:target-4 #\4)
			 (:target-5 #\5)
			 (:target-6 #\6)
			 (:target-7 #\7)
			 (:target-8 #\8)
			 (:target-9 #\9)
			 (t #\Space))))
      (format t "~%")))
  (values world objects path metadata))

(defun dump-injury (world objects path metadata)
  (format t ";; robot injury: ~a~%" (funcall objects :injury))
  (values world objects path metadata))

(defun dump-robot (world objects path metadata)
  (format t ";; robot: ~{~a ~}~%" (funcall objects :robot))
  (values world objects path metadata))

(defun dump-rocks (world objects path metadata)
  (format t ";; rocks: ~{~a~^, ~}~%"
          (sort (copy-list (funcall objects :rock) )
                #'< 
                :key (lambda (coord) 
                       (with-coords (x y) coord 
                         (with-meta-bind (metadata height)
                           (+ (* y height) x))))))
  (values world objects path metadata))

(defun break-script (world objects path metadata)
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (print (funcall objects :injury))
  (break)
  (values world objects path metadata))

(defun debug-script (file path)
  (with-open-file (s file)
    (multiple-value-call
	(make-script (cons #'dump-rocks
			   (cons #'dump-robot
				 (cons #'dump-world
				       (iter (for raction in-sequence path)
					 (ecase raction
					   (#\L (collect #'robot-move-left))
					   (#\R (collect #'robot-move-right))
					   (#\U (collect #'robot-move-up))
					   (#\D (collect #'robot-move-down))
					   (#\W )
					   (#\A ))
					 (collect #'rocks-move)
					 (collect #'water-update)
					 (collect #'dump-rocks)
					 (collect #'dump-robot)
					 (collect #'dump-injury)
					 (collect #'dump-world)
					 (collect #'break-script))))))
      (make-mine s))))

(defun debug-LLLLDDRRRD-script (file)
  (with-open-file (f file)
    (multiple-value-call 
        (make-script (list #'dump-rocks
                           #'dump-robot
			   #'dump-world
                           #'robot-move-left
                           #'rocks-move
                           #'water-update
                           #'dump-rocks
                           #'dump-robot
			   #'dump-world
                           #'robot-move-left
                           #'rocks-move
                           #'water-update
                           #'dump-rocks
                           #'dump-robot                                        
			   #'dump-world
                           #'robot-move-left
                           #'rocks-move
                           #'water-update
                           #'dump-rocks
                           #'dump-robot
			   #'dump-world
                           #'robot-move-left
                           #'rocks-move
                           #'water-update
                           #'dump-rocks
                           #'dump-robot
			   #'dump-world
                           #'robot-move-down
                           #'rocks-move
                           #'water-update
                           #'dump-rocks
                           #'dump-robot
			   #'dump-world
                           #'robot-move-down
                           #'rocks-move
                           #'water-update
                           #'dump-rocks
                           #'dump-robot
			   #'dump-world
                           #'robot-move-right
                           #'rocks-move
                           #'water-update
                           #'dump-rocks
                           #'dump-robot
			   #'dump-world
                           #'robot-move-right
                           #'rocks-move
                           #'water-update
                           #'dump-rocks
                           #'dump-robot
			   #'dump-world
                           #'robot-move-right
                           #'rocks-move
                           #'water-update
                           #'dump-rocks
                           #'dump-robot
			   #'dump-world
                           #'robot-move-down
                           #'rocks-move                                        
                           #'water-update
                           #'dump-rocks                                         
                           #'dump-robot
			   #'dump-world
                           #'break-script
                           ))
      (make-mine f))))
  