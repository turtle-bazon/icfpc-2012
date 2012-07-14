
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

(defun game-loop (world objects path metadata)
  (let ((current-score (score world objects path metadata)))
    (when (and current-score (not *force-shutdown-p*))
      (let ((best (assoc :best metadata)))
        (when (> current-score (second best))
          (setf (second best) current-score
                (third best) path)))
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
            (funcall world-script world objects path metadata))))
  (values world objects path metadata))

