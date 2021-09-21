;;;; forecast.lisp
;;;; Contains all code related to retrieving weather forecasts
;;;; helpful api docs related to this code:
;;;; https://weather-gov.github.io/api/general-faqs
;;;; https://geocoding.geo.census.gov/geocoder/Geocoding_Services_API.html

(defpackage #:simple-weather.forecast
  (:use #:cl)
  (:use #:simple-weather.common)
  (:import-from #:st-json
		#:read-json
		#:getjso*)
  (:import-from #:drakma
		#:http-request)
  (:import-from #:spinneret
		#:with-html-string))

(in-package #:simple-weather.forecast)

(hunchentoot:define-easy-handler (forecasts-handler :uri "/forecasts")(lat long)
  (setf (hunchentoot:content-type*) "text/html")
  (let ((urls (get-weather-gov-forecast-links lat long)))
    (forecasts-page :stylesheets simple-weather.common:*styles* :urls urls))) 

(defun forecasts-page(&key (stylesheets nil)(urls nil))
  (let ((seven-day (first (get-forecast (cdr (first urls))))))
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
	(:div :class "forecast-container"
	      (dolist (day (st-json:getjso* "properties.periods" seven-day))
		(let ((name (st-json:getjso* "name" day)))
		  (:div :class "seven-day-item"
			(:div (:h4 name))
			(:div
			 (local-time:format-timestring nil (local-time:parse-timestring (st-json:getjso* "startTime" day)) :format '(:month "-" :day "-" :year)))
			(:div (:img :src (st-json:getjso* "icon" day)))
			(:div (:strong "Temperature: ")
			      (concatenate 'string
					   (write-to-string (st-json:getjso* "temperature" day))
					   " °"
					   (st-json:getjso* "temperatureUnit" day)))
			(:div (:strong "Windspeed: ")
			      (st-json:getjso* "windSpeed" day))
			(:div (:strong "Wind Direction: ")
			      (st-json:getjso* "windDirection" day))
			(:div (:strong (:i (st-json:getjso* "detailedForecast" day)))
			))))))))))

(defun get-weather-gov-forecast-links(latitude longitude)
  "get the links to forecasts provided by weather.gov for given geocode lat-long"
  (let ((response (multiple-value-bind (resp status)
		      (drakma:http-request (format nil "https://api.weather.gov/points/~a,~a" latitude longitude) :want-stream t)
		    (cons (st-json:read-json resp) status))))
    (handler-case ;we just want nil on failure
	(progn
	  (list 
	   (cons "seven-day" (st-json:getjso* "properties.forecast" (first response)))
	   (cons "hourly"  (st-json:getjso* "properties.forecastHourly" (first response)))))
      (t()))))


(defun get-forecast(url)
  (let ((response (multiple-value-bind(resp status)
		      (drakma:http-request url :want-stream t)
		    (cons (st-json:read-json resp) status))))response))

(defun map-forecast(forecast-array)
  (loop for forecast in forecast-array
	do(print (st-json::jso-alist forecast))))

  (defun status-ok(response)
    (ignore-errors (= (cdr response) 200)))


(defclass forecast()
  ((start-time :initarg :start-time :accessor start-time)
   (end-time :initarg :end-time :accessor end-time)
   (is-day-time :initarg :is-day-time :accessor is-day-time)
   (name :initarg :name :accessor name)
   (icon :initarg :icon :accessor icon)
   (temperature :initarg :temperature :accessor temperature)
   (wind-speed :initarg :wind-speed :accessor wind-speed)
   (wind-direction :initarg :wind-direction :accessor wind-direction)
   (detailed-forecast :initarg :detailed-forecast :accessor detailed-forecast)))


(defun map-json-to-forecasts(forecast-json)
  "maps json array of forecasts to corresponding clos obj"
  (loop for forecast in forecast-json
	collect(make-instance 'forecast
			      :start-time  (local-time:parse-timestring
					    (st-json:getjso "startTime" forecast))
			      :end-time  (local-time:parse-timestring
					  (st-json:getjso "endTime" forecast))
			      :is-day-time (st-json:getjso "isDaytime" forecast)
			      :name (st-json:getjso "name" forecast)
			      :icon (st-json:getjso "icon" forecast)
			      :temperature (st-json:getjso "temperature" forecast)
			      :wind-speed (st-json:getjso "windSpeed" forecast)
			      :wind-direction (st-json:getjso "windDirection" forecast)
			      :detailed-forecast (st-json:getjso "detailedForecast" forecast))))



(defun get-forecast-days(forecasts)
  "returns a list containing unique dates(month-day) from 7-ish day forecast"
  (remove-duplicates
   (loop for day in forecasts
	 collect (local-time:format-timestring nil (start-time day) :format '(:month "-" :day))) :test #'string=))

(defun get-forecast-days-list(forecasts)
  "coverts list of unique dates into list of lists with date as first element"
  (loop for day in (get-forecast-days forecasts)
	collect(list day)))
	  
(defun map-forecasts-to-dates(date-list forecasts)
    (loop for day in date-list
	  do(loop for forecast in forecasts
		  do(progn
		      (when (string= (car day)
				     (local-time:format-timestring
				      nil
				      (start-time forecast) :format '(:month "-" :day)))
		        (push forecast (cdr day) ))))))
