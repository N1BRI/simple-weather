

(defpackage #:simple-weather.common
  (:use #:cl)
  (:import-from #:spinneret
		#:with-html-string)
  (:import-from #:parenscript
		#:ps
		#:getprop)
  (:export *styles*
	   #:show-loader))


(in-package :simple-weather.common)
(defparameter *styles* (list "marx.min.css" "base.css"))

(defun show-loader()
  (ps (setf (parenscript:@ document body inner-h-t-m-l) "<h3>Preparing Forecast...</h3>")))
	     
