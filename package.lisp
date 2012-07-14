(defpackage #:lambda-lifter
  (:use :cl :iterate :metatilities)
  (:shadowing-import-from :metatilities minimize finish)
  (:export))

(in-package :lambda-lifter)

(defvar *force-shutdown-p* nil)

(defmacro with-coords ((x y) coords &body body)
  (with-gensyms (my-coords)
    `(let* ((,my-coords ,coords)
            (,x (realpart ,my-coords))
            (,y (imagpart ,my-coords)))
       ,@body)))

