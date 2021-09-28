;;;; common.lisp contains functions and variables
;;;; frequently needed in most files

(in-package :simple-weather.common)

(defparameter *styles* (list "/app/static/css/weather-icons.min.css"
			     "/app/static/css/marx.min.css"
			     "/app/static/css/base.css"))

	     
 (defmacro with-page ((&key title )(&key styles) &body body)
   `(with-html-string
      (:doctype)
      (:html
       (:head 
	      (:link :rel "icon" :type "image/png" :href "/app/static/map.png")
	      (:meta :name "viewport"
		     :content "width=device-width, initial-scale=1.0")
	      (when ,styles
		(dolist (sheet ,styles)
		  (:link :href sheet :rel "stylesheet")))
              (:title ,title))
       (:body ,@body))))
