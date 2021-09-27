(defpackage #:simple-weather.location
  (:use #:cl)
  (:use #:simple-weather.common)
  (:shadowing-import-from #:spinneret
		#:with-html-string)
  (:shadowing-import-from #:hunchentoot
			  #:define-easy-handler)
  (:import-from #:simple-weather.forecast
		#:get-address-forecast-url-list))
(in-package #:simple-weather.location)


(define-easy-handler (locate :uri "/locate" :default-request-type :post)
    (street city state)
  (setf (hunchentoot:content-type*) "text/html")
  (let ((matches (get-address-forecast-url-list street city state)))
	(location-matches :stylesheets simple-weather.common:*styles* :matches matches)))


(defun location-matches ( &key (stylesheets nil)(matches nil))
  (with-html-string
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
	(cond ((not (null matches))
	       (if (= (length matches) 1)
		   (:h4 (format nil "~a Match Found" (length matches)))
		   (:h4 (format nil "~a Matches Found" (length matches))))
	       (:table
		(dolist (match matches)
		  (:form :action "/forecast" :method "POST"
			 (:tr (:td (:label (first match)))
			 (:input :type "hidden"
				 :name "link"
				 :value (cdr (assoc "seven-day" (second match):test #'string=)))
			 (:td (:button "View Forecast"))))))
		)))))))
		  
