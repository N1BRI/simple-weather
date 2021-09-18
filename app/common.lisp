

(defpackage #:simple-weather.common
  (:use #:cl)
  (:export *styles*))

(in-package :simple-weather.common)
(defparameter *styles* (list "marx.min.css" "base.css"))
