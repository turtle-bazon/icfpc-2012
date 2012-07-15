
(in-package :lambda-lifter)

(defmacro defcollect (name type &key collect-into collect-method)
  `(defun ,name (lx ly)
     (lambda (world objects path metadata)
       (values (lambda (x y)
                 (if (and (= x lx) (= y ly))
                     :robot
                     (funcall world x y)))
               (lambda (type)
                 (case type
                   ,@(when collect-into `((,collect-into
					   ,(ecase collect-method
                                                   (:collect `(cons (complex lx ly) (funcall objects type)))
                                                   (:count `(+ 1 (funcall objects type)))))))
                   (,type (remove (complex lx ly) (funcall objects type)))
                   (t (funcall objects type))))
               path
               metadata))))

(defcollect collect-lambda :lambda :collect-into :collected-lambda :collect-method :collect)
(defcollect collect-lift :open-lambda-lift :collect-into :collected-lifts :collect-method :collect)
(defcollect collect-razor :razor :collect-into :razors :collect-method :count)

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

