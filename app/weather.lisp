(defpackage #:simple-weather.weather
  (:use #:cl)
  (:use #:simple-weather.common)
  (:shadowing-import-from #:spinneret
		#:with-html-string)
  (:shadowing-import-from #:hunchentoot
			  #:define-easy-handler)
  (:shadowing-import-from #:simple-weather.forecast
			  #:get-forecast
			  #:forecast)
  (:shadowing-import-from #:local-time
			  #:format-timestring))
(in-package #:simple-weather.weather)


(define-easy-handler (locate :uri "/forecast" :default-request-type :post)
    (link)
  (setf (hunchentoot:content-type*) "text/html")
  (let ((forecasts (get-forecast link)))
    (forecasts :stylesheets simple-weather.common:*styles* :forecasts forecasts)))

(defun forecasts ( &key (stylesheets nil)(forecasts nil))
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
       (:div :class "main-content"
	     (dolist (forecast forecasts)
	       (:div :class "forecast-item" 
		     (:h4 :class "main-date" (concatenate 'string
				       (format-timestring nil (simple-weather.forecast::start-time
							       (second forecast)) :format '(:long-weekday))
				       " - "
				       (first forecast)))
		     (if (string= (simple-weather.forecast::is-day-time
				   (second forecast)) :TRUE)
			 (:strong :class "Day" "Day")
			 (:strong :class "Night" "Night"))
		     
		     (:br)
		     (:i :class (simple-weather.forecast::icon (second forecast)))
		     (:br)
		     (:strong "Temperature: ") (:span (concatenate 'string  (write-to-string (simple-weather.forecast::temperature (second forecast))) "F")) (:br)
		     (:strong "Wind Speed: ") (:span simple-weather.forecast::(wind-speed (second forecast)))(:br)
		     (:strong "Wind Direction: ") (:span simple-weather.forecast::(wind-direction (second forecast)))(:br)
		     (:p (simple-weather.forecast::detailed-forecast (second forecast)))
		     (when (= (length (cdr forecast)) 2)
		       (:hr)
		       (:div
		        (if (string= (simple-weather.forecast::is-day-time
				   (second (cdr forecast))) :TRUE)
			 (:strong :class "Day" "Day")
			 (:strong :class "Night" "Night"))
		     
		     (:br)
			(:i :class (simple-weather.forecast::icon (second (cdr forecast))))
			(:br)
			(:strong "Temperature: ") (:span (simple-weather.forecast::temperature (second (cdr forecast))))(:br)
			(:strong "Wind Speed: ") (:span (simple-weather.forecast::wind-speed (second (cdr forecast))))(:br)
			(:strong "Wind Direction: ") (:span (simple-weather.forecast::wind-direction (second (cdr forecast))))(:br)
			(:p (simple-weather.forecast::detailed-forecast (second (cdr forecast))))))
		     (:br))))))))

	   
	      

