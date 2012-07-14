
(in-package :lambda-lifter)

(defun lambda-collect (lx ly)
  (lambda (world objects path metadata)
    (values (lambda (x y)
              (if (and (= x lx) (= y ly))
                  :empty
                  (funcall world x y)))
            (lambda (type)
              (if (eq type :collected-lambda)
                  (cons (complex lx ly) (funcall objects type))
                  (funcall objects type)))
            path
            metadata)))

(defun maybe-open-lambda-lift (world objects path metadata)
  (with-coords (llx lly) (first (funcall objects :closed-lambda-lift))
    (let ((lambdas-left (funcall objects :lambda)))
      (values (if lambdas-left
                  world
                  (lambda (x y)
                    (if (and (= x llx) (= y lly))
                        :open-lambda-lift
                        (funcall world x y))))
              (if lambdas-left
                  objects
                  (lambda (type)
                    (case type
                      (:closed-lambda-lift nil)
                      (:open-lambda-lift (list (complex llx lly)))
                      (t (funcall objects type)))))
              path
              metadata))))

