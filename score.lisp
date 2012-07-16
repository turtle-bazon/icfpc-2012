
(in-package :lambda-lifter)

(defun score (world objects path metadata)
  (declare (ignore world))
  (let ((collected-lambdas (funcall objects :collected-lambda))
	(underwater (funcall objects :underwater))
	(injury (funcall objects :injury))
        (score 0))
    (with-meta-bind (metadata waterproof)
      (unless (or (and underwater waterproof (> underwater waterproof)) injury)
	;; 1 point lost for every move made
	(decf score (path-length path))
	;; 25 points gained for every Lambda collected 
	(incf score (* (length collected-lambdas) 25))
        (if (funcall objects :collected-lifts)
            (incf score (* (length collected-lambdas) 50)) ;; 50 extra points per Lambda collected on reaching the winning state
            (incf score (* (length collected-lambdas) 25))) ;; 25 extra points per Lambda collected on executing Abort (this is default for us)
	score))))

