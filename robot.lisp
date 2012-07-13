
(in-package :lambda-lifter)

(defun in-world-p (world x y)
  (multiple-value-bind (w-width w-height)
      (funcall world :dimensions)
    (and (>= x 1)
	 (>= y 1)
	 (<= x w-width)
	 (<= y w-height))))

(defun move-to-valid-p (world x y)
  (let ((element (funcall world :get x y)))
    (member element '(:empty :earth :lambda :open-lambda-lift))))

(defun updated-get (world changes x y)
  (let ((in-changes (assoc `(,x ,y) changes :test #'equal)))
    (if in-changes
	(cdr in-changes)
	(funcall world :get x y))))

(defun updated-set (changes type x y)
  (let ((in-changes (assoc `(,x ,y) changes :test #'equal)))
    (if in-changes
	(setf (cdr in-changes) type)
	(setf changes (cons `((,x ,y) . ,type) changes)))))

(defun change-world (world turn-changes)
  (lambda (command &optional x y)
    (ecase command
      (:get (let ((changed-element (assoc `(,x ,y) turn-changes :test #'equal)))
	      (updated-get world turn-changes x y)))
      (:dimensions (funcall world :dimensions))
      (:fallback world))))

(defun move-up-changes (world)
  (multiple-value-bind (r-x r-y)
      (funcall world :robot-position)
    (let ((r-x~ r-x)
	  (r-y~ (+ r-y 1)))
      (when (and (in-world-p world r-x~ r-y~)
		 (move-to-valid-p world r-x~ r-y~))
	`(((,r-x ,r-y) . :empty)
	  ((,r-x~ ,r-y~) . :robot))))))

(defun test-rock-fall (world changes)
  )

(defun update-map (world changes)
  (multiple-value-bind (w-width w-height)
      (funcall world :dimensions)
    (iter (for y from 1 to w-height)
      (iter (for x from 1 to w-width)
	(or (test-rock-fall world changes))))))

(defun move-up (world)
  (let* ((move-up-changes (move-up-changes world))
         (update-changes (update-map world move-up-changes)))
    (change-world world update-changes)))
