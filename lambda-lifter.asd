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
               (:file "lambda" :depends-on ("map-builder"))
               (:file "rock" :depends-on ("map-builder"))
               (:file "score" :depends-on ("map-builder"))
               (:file "robot" :depends-on ("lambda" "rock"))
               (:file "game" :depends-on ("robot" "score"))
               (:file "main" :depends-on ("game"))))

