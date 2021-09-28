
(defpackage #:simple-weather.common
  (:use #:cl)
  (:import-from #:spinneret
		#:with-html-string
		#:with-html)
  (:import-from #:parenscript
		#:ps
		#:getprop)
  (:export *styles*
	   #:show-loader
	   #:with-page))


(in-package :simple-weather.common)
(defparameter *styles* (list "/app/static/css/weather-icons.min.css"
			     "/app/static/css/marx.min.css"
			     "/app/static/css/base.css"))

(defun show-loader()
  (ps (setf (parenscript:@ document body inner-h-t-m-l)
	      "<h2>Simple U.S. Weather</h2>
	      <h5> Just the U.S. weather</h5>
	      <br>
	      <p>searching for location</p>")))
	     
 (defmacro with-page ((&key title )(&key styles) &body body)
   `(with-html-string
      (:doctype)
      (:html
       (:head
	(:meta (:meta :name "viewport"
		      :content "width=device-width, initial-scale=1.0"))
	       (when ,styles
		 (dolist (sheet ,styles)
		   (:link :href sheet :rel "stylesheet")))
         (:title ,title))
        (:body ,@body))))
