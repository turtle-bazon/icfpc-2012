
(in-package :lambda-lifter)

(defun score (world objects path metadata)
  (declare (ignore world metadata))
  (let ((collected-lambdas (funcall objects :collected-lambda))
        (path-script (funcall path))
        (score 0))
    ;; 1 point lost for every move made
    (decf score (length (remove-if-not (lambda (act) (member act '(:L :R :U :D))) path-script)))
    ;; 25 points gained for every Lambda collected 
    (incf score (* (length collected-lambdas) 25))
    ;; 25 extra points per Lambda collected on executing Abort
    (when (eq (car path-script) :A)
      (incf score (* (length collected-lambdas) 25)))
    ;; 50 extra points per Lambda collected on reaching the winning state
    (when (eql (first (funcall objects :lambda-lift))
               (first (funcall objects :robot)))
      (incf score (* (length collected-lambdas) 50)))
    score))
      