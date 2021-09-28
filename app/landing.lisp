(defpackage #:simple-weather.landing
  (:use #:cl)
  (:use #:simple-weather.common)
  (:import-from #:hunchentoot
		#:define-easy-handler)
  (:import-from #:spinneret
		#:with-html-string))

(in-package #:simple-weather.landing)

(hunchentoot:define-easy-handler (weather :uri "/")()
  (setf (hunchentoot:content-type*) "text/html")
  (with-page(:title "Simple Weather") (:styles *styles*)
       (:div :class "banner" (:a :href "/" (:h2 "Simple U.S. Weather" ))
	(:h5 "Just the U.S. weather"))
	(:br)
	(:div
	 (:form :id "info" :action "/locate" :method "POST"
		(:label :attrs (list :for "street") "Street:")
		(:input :name "street" :id "street" :type "text")
		
		(:label :attrs (list :for "city") "City:")
		(:input :name "city" :id "city" :type "text" )
		
		(:label :attrs (list :for "state") "State:")
		(:input :name "state" :id "state" :type "text")
		(:br)
		(:button :type "submit" :id "submit-btn" "Find Location")))))



         

