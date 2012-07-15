
(in-package :lambda-lifter)
  
(defun make-script (script)
  (lambda (world objects path metadata)
    (iter (for action in script)
          (multiple-value-setq (world objects path metadata)
            (funcall action world objects path metadata))
          (finally (return (values world objects path metadata))))))

(defun make-game-turn (move)
  (let ((go-script 
         (ecase move
           (:cleared nil)
           (:L #'robot-go-left-script)
           (:R #'robot-go-right-script)
           (:D #'robot-go-down-script)
           (:U #'robot-go-up-script)
           (:W #'robot-go-wait-script)
           (:S #'robot-go-razor-script))))
    (lambda (world objects path metadata)
      (when go-script
        (let ((robot-actions (funcall go-script world objects path metadata)))
          (when robot-actions
            (let ((game-turn-script (make-script (append robot-actions
                                                         (list #'rocks-move
                                                               #'beards-growth
                                                               #'water-update)))))
              (funcall game-turn-script world objects path metadata))))))))

(defun make-player (world objects path metadata)
  (lambda (replay-path turn-callback)
    (funcall turn-callback world objects path metadata)
    (multiple-value-bind (turn-world turn-objects turn-path turn-metadata)
        (values world objects path metadata)
      (iter (for move in (reverse (funcall replay-path)))
	    (when (eq move :A)
	       (return (values turn-world turn-objects turn-path turn-metadata)))
            (for turn-proc = (make-game-turn move))
            (multiple-value-bind (new-world new-objects new-path new-metadata)
                (funcall turn-proc turn-world turn-objects turn-path turn-metadata)
              (when (and new-world new-objects new-path new-metadata)
                (multiple-value-setq (turn-world turn-objects turn-path turn-metadata)
                  (values new-world new-objects new-path new-metadata))
                (funcall turn-callback turn-world turn-objects turn-path turn-metadata)))
            (finally (return (values turn-world turn-objects turn-path turn-metadata)))))))
  
(defun visited-p (action sample-dx sample-dy path player)
  (funcall player
           path
           (lambda (world objects path metadata)
             (declare (ignore world metadata))
             (with-robot-coords (rx ry) objects
               (when (and (= rx sample-dx)
                          (= ry sample-dy)
                          (case action
                            ((:W :S) (eq (first (funcall path)) action))
                            (t t)))
                 (return-from visited-p t)))))
  nil)

(defun robot-ai (world objects path metadata player)
;;  (declare (optimize (debug 3)))
  
  (let ((current-target (choose-target world objects path metadata)))
    (unless current-target
      (return-from robot-ai nil))
    (iter (for available-move in '(:L :R :D :U :W))
          (for turn-proc = (make-game-turn available-move))
          (multiple-value-bind (turn-world turn-objects turn-path turn-metadata)
              (funcall turn-proc world objects path metadata)
            (when (and turn-world turn-objects turn-path turn-metadata)
              (with-robot-coords (rx ry) turn-objects
                (for already-visited-p = (visited-p available-move rx ry path player))
                (unless already-visited-p
                  (let ((turn-score (score turn-world turn-objects turn-path turn-metadata))
                        (move available-move))
                    (when turn-score
                      (collect (list move turn-score turn-world turn-objects turn-path turn-metadata) into turns)))))))
          (finally
           (let ((ordered-turns (sort turns (make-positions-comparator current-target))))             
             (iter (for (move turn-score turn-world turn-objects turn-path turn-metadata) in ordered-turns)
                   (game-loop turn-score player turn-world turn-objects turn-path turn-metadata)))))))

(defun update-hiscore (current-score path metadata)
  (let ((best (assoc :best metadata)))
    (when (and best (> current-score (second best)))
      (setf (second best) current-score
            (third best) path))
    (when *force-shutdown-p*
      (let ((best-path (or (third best) (lambda () nil)))
            (best-score (or (second best) 0)))
        (setf (second best) best-score
              (third best) (if (eq (car (funcall best-path)) :A) best-path (lambda () (cons :A (funcall best-path))))))))
  current-score)

(defun game-loop (current-score player world objects path metadata)

  ;; (declare (optimize (debug 3)))
  ;; (dump-world world objects path metadata)
  ;; (format t "Target: ~a; score: ~a; underwater: ~a; path: ~a~%"
  ;;         (choose-target world objects path metadata)
  ;;         (score world objects path metadata)
  ;;         (funcall objects :underwater)
  ;;         (dump-path nil path))
  ;; ;;(sleep 0.2)
  ;; (break)

  (update-hiscore current-score path metadata)

  ;; check for extremal condition
  (when *force-shutdown-p*
    (return-from game-loop))
  
  ;; run robot ai and perform the game turn
  (robot-ai world objects path metadata player))

(defun solve-world (world objects path metadata)
  (let ((player (make-player world objects path metadata)))
    (game-loop 0 player world objects path metadata)
    (let ((best-solve (third (assoc :best metadata))))
      (dump-path t best-solve))))

;; Debugging stuff

(defun dump-world (world objects path metadata)
  (declare (ignorable world objects path metadata))
  (with-meta-bind (metadata width height water flooding)
    (format t ";; growth: ~a~%" (funcall objects :growth))
    (iter
      (with water-level = (+ (if water water 0) (if (and flooding (/= flooding 0)) (floor (path-length path) flooding) 0)))
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
			 (:horock #\@)
			 (t #\Space))))
      (format t "~%")))
  (values world objects path metadata))

(defun dump-injury (world objects path metadata)
  (format t ";; robot injury: ~a~%" (funcall objects :injury))
  (values world objects path metadata))

(defun dump-robot (world objects path metadata)
  (format t ";; robot: ~{~a ~}, injury: ~a, underwater: ~a, score: ~a, razors: ~a~%"
	  (funcall objects :robot) (funcall objects :injury) (funcall objects :underwater)
	  (score world objects path metadata) (funcall objects :razors))
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
    (funcall (multiple-value-call #'make-player (make-mine s))
	     (lambda ()
	       (reverse (iter (for reaction in-sequence path)
			  (collect (form-keyword reaction)))))
	     (lambda (world objects path metadata)
	       (dump-robot world objects path metadata)
	       (dump-world world objects path metadata)
	       (break)))))

