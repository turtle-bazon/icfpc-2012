
(in-package :lambda-lifter)

(defun score (world objects path metadata)
  (declare (ignore world))
  (let ((collected-lambdas (funcall objects :collected-lambda))
	(underwater (or (funcall objects :underwater) 0))
	(injury (funcall objects :injury))
        (path-script (funcall path))
        (score 0))
    (with-meta-bind (metadata waterproof)
      (unless (or (> underwater (or waterproof 0)) injury)
	;; 1 point lost for every move made
	(decf score (path-length path))
	;; 25 points gained for every Lambda collected 
	(incf score (* (length collected-lambdas) 25))
	;; 25 extra points per Lambda collected on executing Abort
	(when (eq (car path-script) :A)
	  (incf score (* (length collected-lambdas) 25)))
	;; 50 extra points per Lambda collected on reaching the winning state
	(when (eql (first (funcall objects :open-lambda-lift))
		   (first (funcall objects :robot)))
	  (incf score (* (length collected-lambdas) 50)))
	score))))
      