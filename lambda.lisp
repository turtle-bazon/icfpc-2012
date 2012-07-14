
(in-package :lambda-lifter)

(defmacro defcollect (name type &key collect-into)
  `(defun ,name (lx ly)
     (lambda (world objects path metadata)
       (values (lambda (x y)
                 (if (and (= x lx) (= y ly))
                     :robot
                     (funcall world x y)))
               (lambda (type)
                 (case type
                   ,@(when collect-into `((,collect-into (cons (complex lx ly) (funcall objects type)))))
                   (,type (remove (complex lx ly) (funcall objects type)))
                   (t (funcall objects type))))
               path
               metadata))))

(defcollect collect-lambda :lambda :collect-into :collected-lambda)
(defcollect collect-lift :open-lambda-lift :collect-into :collected-lifts)

(defun collect-lambda/open-lift (lx ly)  
  (lambda (world objects path metadata)
    (multiple-value-bind (world objects path metadata)
        (funcall (collect-lambda lx ly) world objects path metadata)
      (let ((lambda-lift-coord (first (funcall objects :closed-lambda-lift))))
        (if (and lambda-lift-coord (null (funcall objects :lambda)))
            (with-coords (llx lly) lambda-lift-coord
              (values (lambda (x y)
                        (if (and (= x llx) (= y lly))
                            :open-lambda-lift
                            (funcall world x y)))
                      (lambda (type)
                        (case type
                          (:closed-lambda-lift nil)
                          (:open-lambda-lift (list (complex llx lly)))
                          (t (funcall objects type))))
                      path
                      metadata))
            (values world objects path metadata))))))

