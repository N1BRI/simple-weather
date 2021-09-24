(defpackage #:simple-weather.landing
  (:use #:cl)
  (:use #:simple-weather.common)
  (:import-from #:hunchentoot
		#:define-easy-handler)
  (:import-from #:spinneret
		#:with-html-string))

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
       (:i :class "wi wi-day-sunny wi-flip-vertical")(:br)
       (:i :class "wi wi-day-sleet-storm wi-flip-vertical")(:br)
       (:i :class "wi wi-storm-showers wi-flip-vertical")(:br)
       (:br)
       (:div
	(when errors
	  (:span errors))
	(:form :id "info" :action "/locate" :method "POST"
	 (:label :attrs (list :for "street") "Street:")
	 (:input :name "street" :id "street" :type "text")
	 
	 (:label :attrs (list :for "city") "City:")
	 (:input :name "city" :id "city" :type "text" )
	 
	 (:label :attrs (list :for "state") "State:")
	(:input :name "state" :id "state" :type "text")
	(:br)
	(:button :type "submit" :id "submit-btn" "Find Location"))
	(:script (parenscript:ps
		   (let ((sumbit-btn ((parenscript:@ document get-element-by-id) "submit-btn"))
			 (frm ((parenscript:@ document get-element-by-id) "info")))
			(setf (parenscript:@ sumbit-btn onclick) 
			      (lambda (event)
				 ((parenscript:@ event prevent-default))
				;;((parenscript:@ document body append) frm)
				((parenscript:@ frm submit) )
				(setf (parenscript:@ document body inner-h-t-m-l) "Preparing Forecast...")
			        ))))))))))




         

