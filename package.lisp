(defpackage #:lambda-lifter
  (:use :cl :iterate :metatilities)
  (:shadowing-import-from :metatilities minimize finish)
  (:export))

(in-package :lambda-lifter)

(defvar *force-shutdown-p* nil)
