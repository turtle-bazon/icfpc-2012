
(in-package :lambda-lifter)

(defun find-nearest-lambda (objects)
  (with-robot-coords (rx ry) objects
    (iter (for lambda-coords in (funcall objects :lambda))
          (for x-diff = (- rx (realpart lambda-coords)))
          (for y-diff = (- ry (imagpart lambda-coords)))
          (for sq-distance = (+ (* x-diff x-diff) (* y-diff y-diff)))
          (finding lambda-coords minimizing sq-distance))))

(defun choose-target (objects)
  (let ((lambdas-left (funcall objects :lambda)))
    (if lambdas-left
        (find-nearest-lambda objects)
        (first (funcall objects :open-lambda-lift)))))

(defun visited-p (sample-dx sample-dy path)
  (iter (with dx = 0)
        (with dy = 0)
        (for move in (funcall path))
        (ecase move
          (:L (incf dx))
          (:R (decf dx))
          (:U (decf dy))
          (:D (incf dy))
          (:W (next-iteration)))
        (when (and (= dx sample-dx) (= dy sample-dy))
          (return-from visited-p t))))

(defun robot-ai (world objects path metadata)
  (with-coords (target-x target-y) (choose-target objects)
    (with-robot-coords (rx ry) objects
      (iter (for (dx dy script-builder)
                 in (sort (list (list -1 0 #'robot-go-left-script)
                                (list 1 0 #'robot-go-right-script)
                                (list 0 1 #'robot-go-up-script)
                                (list 0 -1 #'robot-go-down-script))
                          #'<
                          :key (lambda (entry)
                                 (let ((x-diff (- target-x (+ rx (first entry))))
                                       (y-diff (- target-y (+ ry (second entry)))))
                                   (+ (* x-diff x-diff) (* y-diff y-diff))))))              
            (unless (visited-p dx dy path)
              (for script = (funcall script-builder world objects metadata))
              (when script
                (collect script)))))))

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
                                                         #'maybe-open-lambda-lift
                                                         #'game-loop))))
          (funcall world-script world objects path metadata)))
  (values world objects path metadata))

(defun solve-world (world objects path metadata)
  (game-loop world objects path metadata)
  (let ((best-solve (third (assoc :best metadata))))
    (when best-solve
      (format t "~{~a~}" (nreverse (funcall best-solve))))))

;; Debugging stuff

(defun dump-world (world objects path metadata)
  (declare (ignorable world objects path metadata))
  (with-meta-bind (metadata width height)
    (iter (for y from height downto 1)
      (iter (for x from 1 to width)
	(format t "~a" (case (funcall world x y)
			 (:lambda #\\)
			 (:robot #\R)
			 (:rock #\*)
			 (:wall #\#)
			 (:earth #\.)
			 (:open-lambda-lift #\O)
			 (:closed-lambda-lift #\L)
			 (t #\Space))))
      (format t "~%")))
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
  (break)
  (values world objects path metadata))

(defun debug-LLLLDDRRRD-script (file)
  (with-open-file (f file)
    (multiple-value-call 
        (make-script (list #'dump-rocks
                           #'dump-robot
			   #'dump-world
                           #'robot-move-left
                           #'rocks-move
                           #'water-update
                           #'maybe-open-lambda-lift
                           #'dump-rocks
                           #'dump-robot
			   #'dump-world
                           #'robot-move-left
                           #'rocks-move
                           #'water-update
                           #'maybe-open-lambda-lift
                           #'dump-rocks
                           #'dump-robot                                        
			   #'dump-world
                           #'robot-move-left
                           #'rocks-move
                           #'water-update
                           #'maybe-open-lambda-lift
                           #'dump-rocks
                           #'dump-robot
			   #'dump-world
                           #'robot-move-left
                           #'rocks-move
                           #'water-update
                           #'maybe-open-lambda-lift
                           #'dump-rocks
                           #'dump-robot
			   #'dump-world
                           #'robot-move-down
                           #'rocks-move
                           #'water-update
                           #'maybe-open-lambda-lift
                           #'dump-rocks
                           #'dump-robot
			   #'dump-world
                           #'robot-move-down
                           #'rocks-move
                           #'water-update
                           #'maybe-open-lambda-lift
                           #'dump-rocks
                           #'dump-robot
			   #'dump-world
                           #'robot-move-right
                           #'rocks-move
                           #'water-update
                           #'maybe-open-lambda-lift
                           #'dump-rocks
                           #'dump-robot
			   #'dump-world
                           #'robot-move-right
                           #'rocks-move
                           #'water-update
                           #'maybe-open-lambda-lift
                           #'dump-rocks
                           #'dump-robot
			   #'dump-world
                           #'robot-move-right
                           #'rocks-move
                           #'water-update
                           #'maybe-open-lambda-lift
                           #'dump-rocks
                           #'dump-robot
			   #'dump-world
                           #'robot-move-down
                           #'rocks-move                                        
                           #'water-update
                           #'maybe-open-lambda-lift
                           #'dump-rocks                                         
                           #'dump-robot
			   #'dump-world
                           #'break-script
                           ))
      (make-mine f))))
  