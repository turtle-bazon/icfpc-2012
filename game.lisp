
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

(defun estimate-position-weight (target score world objects path metadata)
  (declare (ignorable target score world objects path metadata))
  (with-robot-coords (rx ry) objects
    (with-coords (target-x target-y) target
      (let ((x-diff (- target-x rx))
            (y-diff (- target-y ry)))
        (+ (* x-diff x-diff) (* y-diff y-diff))))))
  
(defun make-script (script)
  (lambda (world objects path metadata)
    (declare (optimize (debug 3)))
    (iter (for action in script)
          (multiple-value-setq (world objects path metadata)
            (funcall action world objects path metadata))
          (finally (return (values world objects path metadata))))))

(defun make-game-turn (move)
  (let ((go-script 
         (ecase move
           (:L #'robot-go-left-script)
           (:R #'robot-go-right-script)
           (:D #'robot-go-down-script)
           (:U #'robot-go-up-script)
           (:W #'robot-go-wait-script)
           (:S #'robot-go-razor-script))))
    (lambda (world objects path metadata)
      (let ((robot-actions (funcall go-script world objects path metadata)))
        (when robot-actions
          (let ((game-turn-script (make-script (append robot-actions
                                                       (list #'rocks-move
                                                             #'beards-growth
                                                             #'water-update)))))
            (funcall game-turn-script world objects path metadata)))))))

(defun robot-ai (world objects path metadata)
  ;; (declare (optimize (debug 3)))
  (let ((current-target (choose-target world objects path metadata)))
    (unless current-target
      (return-from robot-ai nil))
    (iter (for available-move in '(:L :R :D :U :W))
          (for turn-proc = (make-game-turn available-move))
          (for (values turn-world turn-objects turn-path turn-metadata) =
                 (funcall turn-proc world objects path metadata))
          (when (and turn-world turn-objects turn-path turn-metadata)
            (for turn-score = (score turn-world turn-objects turn-path turn-metadata))
            (when turn-score
              (for turn-weight = (estimate-position-weight current-target turn-score turn-world turn-objects turn-path turn-metadata))
              (collect (list turn-weight turn-score turn-world turn-objects turn-path turn-metadata) into turns)))
          (finally
           (break)
           (iter (for (turn-weight turn-score turn-world turn-objects turn-path turn-metadata) in
                      (sort turns #'> :key #'first))
                 (game-loop turn-score turn-world turn-objects turn-path turn-metadata))))))

(defun update-hiscore (current-score path metadata)
  (let ((best (assoc :best metadata)))
    (when (and best (> current-score (second best)))
      (setf (second best) current-score
            (third best) path))
    (when *force-shutdown-p*
      (let ((best-path (third path)))
        (setf (third best) (lambda () (cons :A (funcall best-path)))))))
  current-score)

(defun game-loop (current-score world objects path metadata)

  ;; (declare (optimize (debug 3)))
  ;; (dump-world world objects path metadata)
  ;; (format t "Target: ~a; score: ~a; underwater: ~a; path: ~a~%"
  ;;         (choose-target world objects path metadata)
  ;;         (score world objects path metadata)
  ;;         (funcall objects :underwater)
  ;;         (dump-path nil path))
  ;; ;; (sleep 0.2)
  ;; (break)

  (update-hiscore current-score path metadata)

  ;; check for extremal condition
  (when *force-shutdown-p*
    (return-from game-loop))
  
  ;; run robot ai and perform the game turn
  (robot-ai world objects path metadata))  

(defun solve-world (world objects path metadata)
  (game-loop 0 world objects path metadata)
  (let ((best-solve (third (assoc :best metadata))))
    (dump-path t best-solve)))

;; Debugging stuff

(defun dump-world (world objects path metadata)
  (declare (ignorable world objects path metadata))
  (with-meta-bind (metadata width height water flooding)
    (format t ";; growth: ~a~%" (funcall objects :growth))
    (iter
      (with water-level = (+ (if water water 0) (if flooding (floor (path-length path) flooding) 0)))
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
			 (:beard #\W)
			 (:razor #\!)
			 (t #\Space))))
      (format t "~%")))
  (values world objects path metadata))

(defun dump-injury (world objects path metadata)
  (format t ";; robot injury: ~a~%" (funcall objects :injury))
  (values world objects path metadata))

(defun dump-robot (world objects path metadata)
  (format t ";; robot: ~{~a ~}, injury: ~a, underwater: ~a, score: ~a, razors: ~a~%"
	  (funcall objects :robot) (funcall objects :injury) (funcall objects :underwater)
	  (score world objects path metadata) (funcall objects :razor)) 
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
					   (#\W (collect #'robot-wait))
					   (#\A ))
					 (collect #'rocks-move)
					 (collect #'beards-growth)
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
  