;;; -*- ICFPC 2012: the lambda lifter -*-

(defpackage #:lambda-lifter-asd
  (:use :cl :asdf))

(in-package #:lambda-lifter-asd)

(defsystem lambda-lifter
  :name "lambda-lifter"
  :version "0.1"
  :author "swizard"
  :licence "icfpc"
  :description "ICFPC 2012: the lambda lifter"
  :depends-on (:iterate :metatilities :split-sequence)
  :components ((:file "package")
               (:file "map-builder" :depends-on ("package"))
               (:file "robot" :depends-on ("map-builder"))
               (:file "rock" :depends-on ("map-builder"))
               (:file "score" :depends-on ("map-builder"))
               (:file "game" :depends-on ("robot" "rock" "score"))
               (:file "main" :depends-on ("game"))))

