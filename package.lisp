(defpackage #:lambda-lifter
  (:use :cl :iterate :metatilities)
  (:shadowing-import-from :metatilities minimize finish)
  (:export
    :main))

(in-package :lambda-lifter)

(defparameter *max-ineffective-tries* 12)

(defvar *force-shutdown-p* nil)
(defvar *force-dump-results-p* nil)

(defmacro with-coords ((x y) coords &body body)
  (with-gensyms (my-coords)
    `(let* ((,my-coords ,coords)
            (,x (realpart ,my-coords))
            (,y (imagpart ,my-coords)))
       ,@body)))

(defun robot-coords (objects) (first (funcall objects :robot)))

(defmacro with-robot-coords ((x y) objects &body body)
  `(with-coords (,x ,y) (robot-coords ,objects)
     ,@body))

