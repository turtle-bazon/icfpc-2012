
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

