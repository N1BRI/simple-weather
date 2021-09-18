(defpackage #:simple-weather.landing
  (:use #:cl)
  (:use #:simple-weather.common)
  (:import-from #:hunchentoot
		#:define-easy-handler))

(in-package #:simple-weather.landing)

(hunchentoot:define-easy-handler (weather :uri "/landing")()
  (setf (hunchentoot:content-type*) "text/html")
  (landing-page :stylesheets simple-weather.common:*styles*))

(defun landing-page( &key (stylesheets nil)(errors nil))
  (spinneret:with-html-string
    (:doctype)
    (:html
     (:head
      (:raw "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">")
      (when stylesheets
        (dolist (sheet stylesheets)
	  (:raw (concatenate 'string
			     "<link rel=\"stylesheet\" href=\"/app/static/css/" sheet "\"/>")))))
      (:body
       (:h2 "Simple U.S. Weather" )
       (:h5 "Just the U.S. weather")
       (:br)
       (:div
	(when errors
	  (:span errors))
	(:form :action "/locate" :method "POST"
	 (:label :attrs (list :for "street") "Street:")
	 (:input :name "street" :id "street" :type "text")
	 
	 (:label :attrs (list :for "city") "City:")
	 (:input :name "city" :id "city" :type "text" )
	 
	 (:label :attrs (list :for "state") "State:")
	(:input :name "state" :id "state" :type "text")
	(:br)
	(:button :id "submitBtn" "Find Location" )))))))
