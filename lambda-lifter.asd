;;; -*- ICFPC 2012: the lambda lifter -*-

(defpackage #:lambda-lifter-asd
  (:use :cl :asdf))

(in-package #:lambda-lifter-asd)

(defsystem lambda-lifter
  :name "lambda-lifter"
  :version "0.1"
  :author "skilful&crafty (swizard and turtle)"
  :licence "icfpc"
  :description "ICFPC 2012: the lambda lifter"
  :depends-on (:iterate :metatilities :split-sequence)
  :components ((:file "package")
               (:file "map-builder" :depends-on ("package"))
               (:file "path" :depends-on ("map-builder"))
	       (:file "portals" :depends-on ("path"))
               (:file "lambda" :depends-on ("path"))
               (:file "rock" :depends-on ("path"))
               (:file "score" :depends-on ("path"))
               (:file "robot" :depends-on ("lambda" "rock" "portals"))
	       (:file "water" :depends-on ("robot"))
	       (:file "beard" :depends-on ("path"))
               (:file "game" :depends-on ("robot" "score" "water" "beard"))
               (:file "main" :depends-on ("game"))))

