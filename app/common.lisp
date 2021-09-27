
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
(defparameter *styles* (list "weather-icons.min.css"  "marx.min.css" "base.css"))

(defun show-loader()
  (ps (setf (parenscript:@ document body inner-h-t-m-l)
	      "<h2>Simple U.S. Weather</h2>
	      <h5> Just the U.S. weather</h5>
	      <br>
	      <p>searching for location</p>")))
	     
